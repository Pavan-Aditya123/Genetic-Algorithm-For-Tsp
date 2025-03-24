{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?), (.!=), encode, object)
import Data.Text.Lazy (Text, pack, unpack)
import Network.HTTP.Types (status200, status404)
import System.Random (randomRIO, RandomGen, StdGen, mkStdGen, randomR)
import qualified Data.ByteString.Lazy as B
import Data.List (permutations, foldl', sortBy, take, drop, cycle, maximumBy)
import Data.Function (on)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import System.Random.Shuffle (shuffle')
import Control.Concurrent (forkIO, threadDelay, newMVar, readMVar, modifyMVar_, modifyMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import System.IO (hSetEncoding, stdout, utf8)
import System.CPUTime (getCPUTime)
import Control.Exception (try, IOException)

import qualified GeneticAlgorithm as GA
import GeneticAlgorithm (GAParams(..), GAResult(..), getProgress, getCityNames, defaultParams, GenerationStats(..))

-- | Request data type for the TSP problem
data TSPRequest = TSPRequest { 
    reqCities :: [GA.City],
    reqParameters :: Maybe RequestParams
} deriving (Show, Eq, Generic, NFData)

-- | Parameters that can be specified in the request
data RequestParams = RequestParams {
    populationSize :: Maybe Int,
    generations :: Maybe Int,
    mutationRate :: Maybe Double,
    crossoverRate :: Maybe Double,
    elitismCount :: Maybe Int,
    tournamentSize :: Maybe Int
} deriving (Show, Eq, Generic, NFData)

-- | Response data type for the TSP solution
data TSPResponse = TSPResponse {
    resBestPath :: [Int],           -- Indices of cities in the optimal path
    resBestDistance :: Int,         -- Total distance of the best path
    resGenerationHistory :: [GenerationData], -- Evolution history
    resElapsed :: Double,           -- Time taken in seconds
    resCities :: [GA.City],         -- The cities in the problem
    resParameters :: GAParams       -- Parameters used for the algorithm
} deriving (Show, Eq, Generic, NFData)

-- | Data for each generation for visualization
data GenerationData = GenerationData {
    genGeneration :: Int,
    genBestFitness :: Double,
    genAverageFitness :: Double,
    genBestPath :: [Int]
} deriving (Show, Eq, Generic, NFData)

-- | Running job information
data JobInfo = JobInfo {
    jobId :: String,
    progress :: Int,
    startTime :: Integer
} deriving (Show, Eq)

instance FromJSON GA.City
instance ToJSON GA.City

instance FromJSON TSPRequest where
    parseJSON = Aeson.withObject "TSPRequest" $ \v -> TSPRequest
        <$> v .: "cities"
        <*> v .:? "parameters"

instance FromJSON RequestParams where
    parseJSON = Aeson.withObject "RequestParams" $ \v -> RequestParams
        <$> v .:? "populationSize"
        <*> v .:? "generations"
        <*> v .:? "mutationRate"
        <*> v .:? "crossoverRate"
        <*> v .:? "elitismCount"
        <*> v .:? "tournamentSize"

instance ToJSON TSPResponse where
    toJSON resp = 
        object [ "bestPath" .= resBestPath resp
               , "bestDistance" .= resBestDistance resp
               , "generationHistory" .= resGenerationHistory resp
               , "elapsed" .= resElapsed resp
               , "cities" .= resCities resp
               , "parameters" .= object [ "populationSize" .= GA.populationSize (resParameters resp)
                                        , "generations" .= GA.maxGenerations (resParameters resp)
                                        , "mutationRate" .= GA.mutationRate (resParameters resp)
                                        , "crossoverRate" .= GA.crossoverRate (resParameters resp)
                                        , "elitismCount" .= GA.elitismCount (resParameters resp)
                                        , "tournamentSize" .= GA.tournamentSize (resParameters resp)
                                        ]
               ]

instance ToJSON GenerationData where
    toJSON gen =
        object [ "generation" .= genGeneration gen
               , "bestFitness" .= genBestFitness gen
               , "averageFitness" .= genAverageFitness gen
               , "bestPath" .= genBestPath gen
               ]

-- Create GA parameters from request
createParams :: TSPRequest -> GAParams
createParams req = 
    let reqParams = fromMaybe defaultRequestParams (reqParameters req)
    in GAParams {
        GA.populationSize = fromMaybe (GA.populationSize defaultParams) (Main.populationSize reqParams),
        GA.maxGenerations = fromMaybe (GA.maxGenerations defaultParams) (generations reqParams),
        GA.mutationRate = fromMaybe (GA.mutationRate defaultParams) (Main.mutationRate reqParams),
        GA.crossoverRate = fromMaybe (GA.crossoverRate defaultParams) (Main.crossoverRate reqParams),
        GA.elitismCount = fromMaybe (GA.elitismCount defaultParams) (Main.elitismCount reqParams),
        GA.tournamentSize = fromMaybe (GA.tournamentSize defaultParams) (Main.tournamentSize reqParams)
    }

-- Default request parameters
defaultRequestParams :: RequestParams
defaultRequestParams = RequestParams {
    Main.populationSize = Nothing,
    generations = Nothing,
    Main.mutationRate = Nothing,
    Main.crossoverRate = Nothing,
    Main.elitismCount = Nothing,
    Main.tournamentSize = Nothing
}

-- Main function that handles CLI or web server
main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    case args of
        ["solve", inputFile, outputFile] -> solveFromFiles inputFile outputFile
        _ -> startServer

-- Start the web server
startServer :: IO ()
startServer = do
    -- Create a map to store running jobs
    jobsMVar <- newMVar ([] :: [JobInfo])

    putStrLn "⚡ Server running at http://localhost:3000"
    putStrLn "📊 TSP Genetic Algorithm Visualization"

    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "public")
        
        -- Serve the main page
        get "/" $ file "public/index.html"
        
        -- API endpoint to get job progress
        get "/progress/:jobId" $ do
            jobId <- param "jobId"
            jobs <- liftIO $ readMVar jobsMVar
            case filter (\j -> jobId == Main.jobId j) jobs of
                [] -> json $ object ["progress" .= (0 :: Int)]
                (job:_) -> json $ object ["progress" .= progress job]
        
        -- API endpoint to solve TSP
        post "/solve-tsp" $ do
            startTime <- liftIO getCPUTime
            tspRequest <- jsonData :: ActionM TSPRequest
            
            -- Generate a job ID
            jobId <- liftIO $ show <$> getCPUTime
            
            -- Create a progress tracker
            progressMVar <- liftIO $ newMVar (0 :: Int)
            
            -- Add job to the job list
            liftIO $ modifyMVar_ jobsMVar $ \jobs -> 
                return $ JobInfo jobId 0 startTime : jobs
            
            -- Create parameters from request
            let citiesList = reqCities tspRequest
                params = createParams tspRequest
            
            liftIO $ putStrLn $ "Received TSP request with " ++ show (length citiesList) ++ " cities"
            liftIO $ putStrLn $ "Parameters: populationSize=" ++ show (GA.populationSize params) ++ 
                                ", generations=" ++ show (GA.maxGenerations params)
            
            -- Fork a thread to run the GA algorithm
            _ <- liftIO $ forkIO $ do
                -- Run the genetic algorithm
                result <- GA.runParallelGA citiesList params
                endTime <- getCPUTime
                let timeElapsed = fromIntegral (endTime - startTime) / 1e12  -- Convert picoseconds to seconds
                
                -- Create generation history
                let genHistory = map (\stat -> GenerationData {
                        genGeneration = generation stat,
                        genBestFitness = bestFitnessGen stat,
                        genAverageFitness = avgFitnessGen stat,
                        genBestPath = bestRouteGen stat
                    }) (allGenerationStats result)
                
                -- Create response
                let response = TSPResponse {
                        resBestPath = bestRoute result,
                        resBestDistance = bestDistance result,
                        resGenerationHistory = genHistory,
                        resElapsed = timeElapsed,
                        resCities = citiesList,
                        resParameters = params
                    }
                
                -- Save the result to a file
                B.writeFile "public/output.json" (encode response)
                
                -- Update job progress to 100%
                modifyMVar_ progressMVar $ \_ -> return 100
            
            -- Start tracking job progress updates
            liftIO $ forkIO $ do
                let updateInterval = 500000 -- 0.5 second
                let updateProgress = do
                        currentProgress <- readMVar progressMVar
                        modifyMVar_ jobsMVar $ \jobs ->
                            return $ map (\j -> if Main.jobId j == jobId 
                                                then j { progress = currentProgress }
                                                else j) jobs
                        if currentProgress < 100
                            then threadDelay updateInterval >> updateProgress
                            else return ()
                updateProgress
            
            -- Return the job ID for the client to track progress
            json $ object ["jobId" .= jobId]
        
        -- API endpoint to get the result
        get "/result" $ do
            resultExists <- liftIO $ doesFileExist "public/output.json"
            if resultExists
                then do
                    resultData <- liftIO $ B.readFile "public/output.json"
                    raw resultData
                else status status404 >> text "No results available yet"

-- Check if a file exists
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    result <- try $ B.readFile path
    case result of
        Left (_ :: IOException) -> return False
        Right _ -> return True

-- Solve TSP from input/output files (for CLI usage)
solveFromFiles :: FilePath -> FilePath -> IO ()
solveFromFiles inputFile outputFile = do
    putStrLn $ "Reading input from " ++ inputFile
    
    -- Read input data
    inputBytes <- B.readFile inputFile
    let maybeRequest = Aeson.decode inputBytes :: Maybe TSPRequest
    
    case maybeRequest of
        Nothing -> putStrLn "Error: Could not parse input file"
        Just request -> do
            startTime <- getCPUTime
            let citiesList = reqCities request
                params = createParams request
            
            putStrLn $ "Solving TSP for " ++ show (length citiesList) ++ " cities"
            putStrLn $ "Using parameters: " ++ show params
            
            -- Run the genetic algorithm
            result <- GA.runParallelGA citiesList params
            endTime <- getCPUTime
            let timeElapsed = fromIntegral (endTime - startTime) / 1e12  -- Convert picoseconds to seconds
            
            -- Create generation history
            let genHistory = map (\stat -> GenerationData {
                    genGeneration = generation stat,
                    genBestFitness = bestFitnessGen stat,
                    genAverageFitness = avgFitnessGen stat,
                    genBestPath = bestRouteGen stat
                }) (allGenerationStats result)
            
            -- Create response
            let response = TSPResponse {
                    resBestPath = bestRoute result,
                    resBestDistance = bestDistance result,
                    resGenerationHistory = genHistory,
                    resElapsed = timeElapsed,
                    resCities = citiesList,
                    resParameters = params
                }
            
            -- Write output to file
            B.writeFile outputFile (encode response)
            
            putStrLn $ "Solution saved to " ++ outputFile
            putStrLn $ "Best distance: " ++ show (bestDistance result)
            putStrLn $ "Time taken: " ++ show timeElapsed ++ " seconds"
