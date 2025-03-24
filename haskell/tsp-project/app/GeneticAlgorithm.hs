{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GeneticAlgorithm where

import System.Random (RandomGen, randomR, mkStdGen, randomRIO)
import Control.Parallel.Strategies (parMap, rdeepseq, using, rpar, parList)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.List (sortBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad (replicateM)
import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, readMVar, modifyMVar_, modifyMVar)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)

-- |Represents a city in the TSP problem
data City = City { 
    name :: String, 
    x :: Int, 
    y :: Int 
} deriving (Show, Eq, Generic, NFData)

-- |Type for a route (a permutation of city indices)
type Route = [Int]
type Population = [Route]
type DistanceMatrix = [[Int]]

-- |Parameters for the genetic algorithm
data GAParams = GAParams {
    populationSize :: Int,
    maxGenerations :: Int,
    mutationRate :: Double,
    crossoverRate :: Double,
    elitismCount :: Int,
    tournamentSize :: Int
} deriving (Show, Generic, NFData)

-- |Default parameters
defaultParams :: GAParams
defaultParams = GAParams {
    populationSize = 100,
    maxGenerations = 100,
    mutationRate = 0.01,
    crossoverRate = 0.9,
    elitismCount = 2,
    tournamentSize = 5
}

-- |Result of the genetic algorithm
data GAResult = GAResult {
    bestRoute :: Route,
    bestDistance :: Int,
    generationReached :: Int,
    bestFitness :: Double,
    avgFitness :: Double,
    allGenerationStats :: [GenerationStats]
} deriving (Show, Generic, NFData)

-- |Statistics for each generation
data GenerationStats = GenerationStats {
    generation :: Int,
    bestRouteGen :: Route,
    bestDistanceGen :: Int,
    bestFitnessGen :: Double,
    avgFitnessGen :: Double
} deriving (Show, Generic, NFData)

-- |Calculate Euclidean distance between two cities
distance :: City -> City -> Int
distance (City _ x1 y1) (City _ x2 y2) = 
    round $ sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

-- |Generate a distance matrix for all city pairs
generateDistanceMatrix :: [City] -> DistanceMatrix
generateDistanceMatrix cities = 
    [[ if i == j then 0 else distance (cities !! i) (cities !! j) | j <- [0..n-1]] | i <- [0..n-1]]
  where
    n = length cities

-- |Calculate the total distance of a route
routeDistance :: DistanceMatrix -> Route -> Int
routeDistance distMatrix route = 
    sum [distMatrix !! (route !! i) !! (route !! next) | i <- [0..length route - 1]]
  where
    next i = (i + 1) `mod` length route

-- |Calculate fitness of a route (inverse of distance)
fitness :: DistanceMatrix -> Route -> Double
fitness distMatrix route = 1.0 / fromIntegral (routeDistance distMatrix route)

-- |Generate a random route
generateRandomRoute :: RandomGen g => Int -> g -> (Route, g)
generateRandomRoute n g = 
    let (route, g') = shuffle [0..n-1] g
    in (route, g')

-- |Fisher-Yates shuffle
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle [x] g = ([x], g)
shuffle xs g = 
    let (i, g') = randomR (0, length xs - 1) g
        (front, x:back) = splitAt i xs
        (rest, g'') = shuffle (front ++ back) g'
    in (x:rest, g'')

-- |Generate an initial population of random routes
initialPopulation :: RandomGen g => GAParams -> Int -> g -> Population
initialPopulation params numCities g =
    go (populationSize params) g []
  where
    go 0 _ acc = acc
    go n gen acc =
        let (route, gen') = generateRandomRoute numCities gen
        in go (n-1) gen' (route:acc)

-- |Select parents using tournament selection
tournamentSelection :: RandomGen g => GAParams -> DistanceMatrix -> Population -> g -> (Route, g)
tournamentSelection params distMatrix pop g =
    let tournSize = tournamentSize params
        (indices, g') = randomIndices (length pop) tournSize g
        tournament = map (pop !!) indices
        best = maximumBy (comparing (fitness distMatrix)) tournament
    in (best, g')
  where
    randomIndices :: RandomGen g => Int -> Int -> g -> ([Int], g)
    randomIndices popSize tournSize gen =
        foldr (\_ (acc, g) -> 
                let (idx, g') = randomR (0, popSize - 1) g
                in (idx:acc, g'))
              ([], gen)
              [1..tournSize]

-- |Order Crossover (OX) operator
orderCrossover :: RandomGen g => Route -> Route -> g -> (Route, g)
orderCrossover parent1 parent2 g =
    let n = length parent1
        (start, g') = randomR (0, n - 1) g
        (end, g'') = randomR (start, n - 1) g'
        
        -- Get the segment from parent1
        segment = take (end - start + 1) $ drop start parent1
        
        -- Get the remaining elements in the order they appear in parent2
        remaining = filter (`notElem` segment) parent2
        
        -- Construct the child
        child = take start remaining ++ segment ++ drop start remaining
    in (child, g'')

-- |Swap Mutation operator
swapMutation :: RandomGen g => Double -> Route -> g -> (Route, g)
swapMutation rate route g =
    let (p, g') = randomR (0.0, 1.0) g
    in if p < rate
       then 
           let n = length route
               (i, g'') = randomR (0, n - 1) g'
               (j, g''') = randomR (0, n - 1) g''
               route' = swapElements i j route
           in (route', g''')
       else (route, g')
  where
    swapElements i j xs =
        let elemI = xs !! i
            elemJ = xs !! j
            replace pos old = if pos == i then elemJ else if pos == j then elemI else old
        in map (\(pos, val) -> replace pos val) (zip [0..] xs)

-- |Inversion Mutation operator
inversionMutation :: RandomGen g => Double -> Route -> g -> (Route, g)
inversionMutation rate route g =
    let (p, g') = randomR (0.0, 1.0) g
    in if p < rate
       then 
           let n = length route
               (i, g'') = randomR (0, n - 1) g'
               (j, g''') = randomR (0, n - 1) g''
               (start, end) = if i <= j then (i, j) else (j, i)
               route' = take start route ++ 
                        reverse (take (end - start + 1) (drop start route)) ++ 
                        drop (end + 1) route
           in (route', g''')
       else (route, g')

-- |Create a new generation through selection, crossover, and mutation
evolvePopulation :: RandomGen g => GAParams -> DistanceMatrix -> Population -> g -> (Population, g)
evolvePopulation params distMatrix pop g =
    let eliteCount = elitismCount params
        popSize = populationSize params
        
        -- Sort population by fitness (descending)
        sortedPop = sortBy (flip $ comparing $ fitness distMatrix) pop
        
        -- Keep the elite individuals
        elites = take eliteCount sortedPop
        
        -- Generate the rest of the population
        (newPop, g') = createNewIndividuals params distMatrix sortedPop (popSize - eliteCount) g
    in (elites ++ newPop, g')

-- |Create new individuals through crossover and mutation
createNewIndividuals :: RandomGen g => GAParams -> DistanceMatrix -> Population -> Int -> g -> ([Route], g)
createNewIndividuals _ _ _ 0 g = ([], g)
createNewIndividuals params distMatrix pop n g =
    let -- Select two parents
        (parent1, g') = tournamentSelection params distMatrix pop g
        (parent2, g'') = tournamentSelection params distMatrix pop g'
        
        -- Crossover
        (p, g''') = randomR (0.0, 1.0) g''
        (child, g4) = if p < crossoverRate params
                      then orderCrossover parent1 parent2 g'''
                      else (parent1, g''')
        
        -- Mutation (apply both types of mutation)
        (mutated1, g5) = swapMutation (mutationRate params) child g4
        (mutated2, g6) = inversionMutation (mutationRate params / 2) mutated1 g5
        
        -- Recursively create the rest
        (rest, g7) = createNewIndividuals params distMatrix pop (n-1) g6
    in (mutated2:rest, g7)

-- |Run the genetic algorithm in parallel
runParallelGA :: [City] -> GAParams -> IO GAResult
runParallelGA cities params = do
    let numCities = length cities
        distMatrix = generateDistanceMatrix cities
        seed = 42
        initialPop = initialPopulation params numCities (mkStdGen seed)
        
    -- Create progress MVar for tracking evolution
    progressMVar <- newMVar (0 :: Int)
    
    -- Run the evolution process
    (finalPop, stats) <- evolvePar params distMatrix initialPop (maxGenerations params) progressMVar 0 []
    
    -- Get the best route from the final population
    let bestIndividual = minimumBy (comparing (routeDistance distMatrix)) finalPop
        bestDist = routeDistance distMatrix bestIndividual
        bestFit = fitness distMatrix bestIndividual
        avgFit = sum (map (fitness distMatrix) finalPop) / fromIntegral (length finalPop)
        genReached = maxGenerations params
    
    return $ GAResult {
        bestRoute = bestIndividual,
        bestDistance = bestDist,
        generationReached = genReached,
        bestFitness = bestFit,
        avgFitness = avgFit,
        allGenerationStats = stats
    }

-- |Run the genetic algorithm in parallel for multiple generations
evolvePar :: GAParams -> DistanceMatrix -> Population -> Int -> MVar Int -> Int -> [GenerationStats] -> IO (Population, [GenerationStats])
evolvePar params distMatrix pop 0 _ currentGen stats = do
    return (pop, reverse stats)
evolvePar params distMatrix pop remainingGens progressMVar currentGen stats = do
    -- Update progress
    let progress = truncate $ 100 * (fromIntegral (maxGenerations params - remainingGens) / fromIntegral (maxGenerations params))
    modifyMVar_ progressMVar (const $ return progress)
    
    -- Calculate fitness values in parallel
    let fitnessValues = map (fitness distMatrix) pop `using` parList rdeepseq
    
    -- Get current stats
    let bestIndividual = fst $ maximumBy (comparing snd) (zip pop fitnessValues)
        bestDist = routeDistance distMatrix bestIndividual
        bestFit = maximum fitnessValues
        avgFit = sum fitnessValues / fromIntegral (length fitnessValues)
        genStats = GenerationStats {
            generation = currentGen,
            bestRouteGen = bestIndividual,
            bestDistanceGen = bestDist,
            bestFitnessGen = bestFit,
            avgFitnessGen = avgFit
        }
    
    -- Evolve population to create next generation (using unsafePerformIO for randomness in pure context)
    let (newPop, _) = unsafePerformIO $ do
            g <- mkStdGen <$> randomRIO (1, 1000000)
            return $ evolvePopulation params distMatrix pop g
        
    -- Continue evolution with next generation
    evolvePar params distMatrix newPop (remainingGens - 1) progressMVar (currentGen + 1) (genStats:stats)

-- |Get the current progress of the algorithm
getProgress :: MVar Int -> IO Int
getProgress = readMVar

-- |Helper function to get the names of cities in a route
getCityNames :: [City] -> Route -> [String]
getCityNames cities route = map (name . (cities !!)) route 