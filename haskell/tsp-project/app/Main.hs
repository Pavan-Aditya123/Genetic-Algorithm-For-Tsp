{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?), (.!=), encode, object)
import Data.Text.Lazy (Text, pack, unpack)
import Network.HTTP.Types (status200)
import System.Random (randomRIO, RandomGen, StdGen, mkStdGen, randomR)
import qualified Data.ByteString.Lazy as B
import Data.List (permutations, foldl', sortBy, take, drop, cycle)
import Data.Function (on)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffle')
import Control.Concurrent (forkIO, threadDelay, newMVar, readMVar, modifyMVar_)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

import GeneticAlgorithm (City(..), GAParams(..), GAResult(..), runParallelGA, getProgress, getCityNames, defaultParams)

-- Define a City type to represent the coordinates
data City = City { name :: String, x :: Int, y :: Int } deriving (Show, Eq, Generic, NFData)

-- Define request and response data types
data TSPRequest = TSPRequest { 
    cities :: [City],
    populationSize :: Maybe Int,
    generations :: Maybe Int,
    mutationRate :: Maybe Double,
    crossoverRate :: Maybe Double,
    elitismCount :: Maybe Int,
    tournamentSize :: Maybe Int
} deriving (Show, Eq, Generic, NFData)

data TSPResponse = TSPResponse { 
    path :: [String],
    distance :: Int,
    generation :: Int,
    bestFitness :: Double,
    averageFitness :: Double
} deriving (Show, Eq, Generic, NFData)

instance FromJSON TSPRequest where
    parseJSON (Aeson.Object v) = TSPRequest 
        <$> v .: "cities"
        <*> v .:? "populationSize"
        <*> v .:? "generations"
        <*> v .:? "mutationRate"
        <*> v .:? "crossoverRate"
        <*> v .:? "elitismCount"
        <*> v .:? "tournamentSize"
    parseJSON _ = fail "Expected an object for TSPRequest"

instance ToJSON TSPResponse where
    toJSON (TSPResponse path distance generation bestFitness avgFitness) = 
        object [ "path" .= path
               , "distance" .= distance
               , "generation" .= generation
               , "bestFitness" .= bestFitness
               , "averageFitness" .= avgFitness
               ]

instance FromJSON City where
    parseJSON (Aeson.Object v) = City <$> v .: "name" <*> v .: "x" <*> v .: "y"
    parseJSON _ = fail "Expected an object for City"

instance ToJSON City where
    toJSON (City name x y) = object ["name" .= name, "x" .= x, "y" .= y]

-- Genetic Algorithm parameters
data GAParams = GAParams
    { populationSize :: Int
    , maxGenerations :: Int
    , mutationRate :: Double
    , crossoverRate :: Double
    , elitismCount :: Int
    , tournamentSize :: Int
    } deriving (Show)

-- Individual in the population
type Individual = [Int]
type Population = [Individual]
type Fitness = Double

-- Create GA parameters from request
createParams :: TSPRequest -> GAParams
createParams req = GAParams {
    populationSize = fromMaybe (populationSize defaultParams) (Main.populationSize req),
    maxGenerations = fromMaybe (maxGenerations defaultParams) (generations req),
    mutationRate = fromMaybe (mutationRate defaultParams) (Main.mutationRate req),
    crossoverRate = fromMaybe (crossoverRate defaultParams) (Main.crossoverRate req),
    elitismCount = fromMaybe (elitismCount defaultParams) (Main.elitismCount req),
    tournamentSize = fromMaybe (tournamentSize defaultParams) (Main.tournamentSize req)
}

-- Main function that handles CLI or web server
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["solve", inputFile, outputFile] -> solveFromFiles inputFile outputFile
        _ -> startServer

-- Start the web server
startServer :: IO ()
startServer = do
    -- Create a shared progress tracker
    progressRef <- newMVar (0 :: Int)

    scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "public")
        
        get "/" $ file "public/index.html"
        
        get "/progress" $ do
            progress <- liftIO $ readMVar progressRef
            json $ object ["progress" .= progress]
        
        post "/solve-tsp" $ do
            tspRequest <- jsonData :: ActionM TSPRequest
            let citiesList = cities tspRequest
                params = createParams tspRequest
            
            liftIO $ putStrLn $ "Received cities: " ++ show (length citiesList)
            liftIO $ putStrLn $ "Using parameters: " ++ show params
            
            -- Run genetic algorithm
            result <- liftIO $ runParallelGA citiesList params
            
            -- Create response
            let response = TSPResponse {
                path = getCityNames citiesList (bestRoute result),
                distance = bestDistance result,
                generation = generationReached result,
                bestFitness = bestFitness result,
                averageFitness = avgFitness result
            }
            
            -- Save result to file for later retrieval
            liftIO $ B.writeFile "public/output.json" (encode response)
            
            json response

-- Solve TSP from input/output files (for CLI usage)
solveFromFiles :: String -> String -> IO ()
solveFromFiles inputFile outputFile = do
    putStrLn $ "Reading input from " ++ inputFile
    
    -- Read input file
    inputData <- B.readFile inputFile
    let maybeRequest = Aeson.decode inputData :: Maybe TSPRequest
    
    case maybeRequest of
        Nothing -> putStrLn "Error: Could not parse input file"
        Just request -> do
            let citiesList = cities request
                params = createParams request
            
            putStrLn $ "Solving TSP for " ++ show (length citiesList) ++ " cities"
            putStrLn $ "Using parameters: " ++ show params
            
            -- Run genetic algorithm
            result <- runParallelGA citiesList params
            
            -- Create and save response
            let response = TSPResponse {
                path = getCityNames citiesList (bestRoute result),
                distance = bestDistance result,
                generation = generationReached result,
                bestFitness = bestFitness result,
                averageFitness = avgFitness result
            }
            
            B.writeFile outputFile (encode response)
            putStrLn $ "Solution saved to " ++ outputFile
            putStrLn $ "Best distance: " ++ show (bestDistance result)

-- Parallel Genetic Algorithm implementation
runParallelGA :: [City] -> GAParams -> IO GAResult
runParallelGA cities params = do
    let distances = generateDistanceMatrix cities
    let (bestPath, bestDist, gen, bestFit, avgFit) = 
            evolve (maxGenerations params) (generateInitialPopulation (populationSize params) (length cities)) distances params
    return GAResult {
        bestRoute = bestPath,
        bestDistance = bestDist,
        generationReached = gen,
        bestFitness = bestFit,
        avgFitness = avgFit
    }
  where
    evolve gen pop distances params = 
        let (finalPath, finalDist, gen, bestFit, avgFit) = 
                evolveHelper gen pop distances params
        in (finalPath, finalDist, gen, bestFit, avgFit)

    evolveHelper 0 pop distances params = 
        let bestIdx = maximumBy (compare `on` (fitness distances)) [0..length pop - 1]
            bestPath = pop !! bestIdx
            bestDist = pathDistance distances bestPath
            bestFit = fitness distances bestPath
            avgFit = sum (map (fitness distances) pop) / fromIntegral (length pop)
        in (bestPath, bestDist, 0, bestFit, avgFit)

    evolveHelper gen pop distances params = do
        -- Update progress
        modifyMVar_ progressRef $ \progress -> return $ progress + 1
        
        -- Select parents using tournament selection
        let parents = selectParents pop distances (tournamentSize params)
        
        -- Create new population through crossover and mutation
        let newPop = createNewPopulation parents distances params
        
        -- Calculate new fitnesses in parallel
        let newFitnesses = parMap rdeepseq (fitness distances) newPop
        
        -- Continue evolution
        evolve (gen - 1) newPop distances params

-- Generate initial population
generateInitialPopulation :: Int -> Int -> Population
generateInitialPopulation size n = 
    [shuffle' [0..n-1] n (mkStdGen i) | i <- [0..size-1]]

-- Calculate fitness of an individual
fitness :: [[Int]] -> Individual -> Fitness
fitness distances path = 
    1.0 / (fromIntegral $ pathDistance distances path)

-- Calculate total distance of a path
pathDistance :: [[Int]] -> [Int] -> Int
pathDistance distances path = 
    sum [distances !! (path !! i) !! (path !! (i + 1)) | i <- [0..(length path - 2)]] +
    distances !! (path !! (length path - 1)) !! (path !! 0)

-- Tournament selection
selectParents :: Population -> [[Int]] -> Int -> Population
selectParents pop distances size = 
    [pop !! tournamentSelect distances | _ <- [1..size]]
  where
    tournamentSelect :: [[Int]] -> Int
    tournamentSelect ds = 
        let candidates = take 3 $ shuffle' [0..length ds - 1] (length ds) (mkStdGen 0)
            winner = maximumBy (compare `on` (ds !!)) candidates
        in winner

-- Create new population through crossover and mutation
createNewPopulation :: Population -> [[Int]] -> GAParams -> Population
createNewPopulation parents distances params = 
    let elite = take (elitismCount params) $ sortBy (compare `on` (fitness distances)) parents
        offspring = [crossover (parents !! i) (parents !! (i + 1)) | i <- [0,2..length parents - 2]]
        mutated = map (mutate (mutationRate params)) offspring
    in elite ++ mutated

-- Crossover operator (Order Crossover)
crossover :: Individual -> Individual -> Individual
crossover parent1 parent2 = 
    let len = length parent1
        (start, end) = randomR (0, len - 1) (mkStdGen 0)
        segment = take (end - start + 1) $ drop start parent1
        remaining = filter (`notElem` segment) parent2
    in take start remaining ++ segment ++ drop start remaining

-- Mutation operator (Swap Mutation)
mutate :: Double -> Individual -> Individual
mutate rate individual = 
    if randomRIO (0, 1) < rate
    then let (i, j) = randomR (0, length individual - 1) (mkStdGen 0)
             mutated = individual
         in take i mutated ++ [mutated !! j] ++ drop (i + 1) (take j mutated) ++ [mutated !! i] ++ drop (j + 1) mutated
    else individual

-- Generate the distance matrix between cities
generateDistanceMatrix :: [City] -> [[Int]]
generateDistanceMatrix cities = 
    [[ if i == j then 0 else distance (cities !! i) (cities !! j) | j <- [0..n-1]] | i <- [0..n-1]]
  where
    n = length cities

-- Euclidean distance between two cities
distance :: City -> City -> Int
distance (City _ x1 y1) (City _ x2 y2) =
    round (sqrt (fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)))
