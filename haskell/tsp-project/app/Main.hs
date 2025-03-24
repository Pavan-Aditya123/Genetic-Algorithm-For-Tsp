{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), encode)
import Data.Text.Lazy (Text, pack, unpack)
import Network.HTTP.Types (status200)
import System.Random (randomRIO)
import qualified Data.ByteString.Lazy as B
import Data.List (permutations, foldl')
import Data.Function (on)

-- Define a City type to represent the coordinates
data City = City { name :: String, x :: Int, y :: Int } deriving (Show, Eq)

-- Define request and response data types
data TSPRequest = TSPRequest { cities :: [City] } deriving (Show, Eq)
data TSPResult = TSPResult { path :: [String], distance :: Int } deriving (Show, Eq)

instance FromJSON City where
    parseJSON (Aeson.Object v) = City <$> v .: "name" <*> v .: "x" <*> v .: "y"
    parseJSON _ = fail "Expected an object with 'name', 'x', and 'y' fields"

instance ToJSON City where
    toJSON (City name x y) = Aeson.object ["name" .= name, "x" .= x, "y" .= y]

instance FromJSON TSPRequest where
    parseJSON (Aeson.Object v) = TSPRequest <$> v .: "cities"
    parseJSON _ = fail "Expected an object with a 'cities' field"

instance ToJSON TSPResult where
    toJSON (TSPResult path distance) = Aeson.object ["path" .= path, "distance" .= distance]

-- Main server function
main :: IO ()
main = scotty 3000 $ do
    -- Serve static files (e.g., HTML, CSS)
    middleware $ staticPolicy (noDots >-> addBase "public")
    
    -- Endpoint to solve TSP
    post "/solve-tsp" $ do
        tspRequest <- jsonData :: ActionM TSPRequest
        let citiesList = cities tspRequest
        liftIO $ putStrLn $ "Received cities: " ++ show citiesList

        -- Generate a distance matrix based on cities' coordinates
        let distances = generateDistanceMatrix citiesList
        
        -- Solve the TSP using the brute-force approach (can be replaced with GA later)
        let (path, dist) = solveTSP distances (length citiesList)
        
        -- Save result to output.json
        let result = TSPResult (map (name . (citiesList !!)) path) dist
        liftIO $ B.writeFile "output.json" (encode result)
        
        -- Send result as response
        json result

-- Generate the distance matrix between cities
generateDistanceMatrix :: [City] -> [[Int]]
generateDistanceMatrix cities = [[ if i == j then 0 else distance (cities !! i) (cities !! j) | j <- [0..n-1]] | i <- [0..n-1]]
  where
    n = length cities

-- Euclidean distance between two cities
distance :: City -> City -> Int
distance (City _ x1 y1) (City _ x2 y2) =
    round (sqrt (fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)))

-- Solve the TSP using brute-force (for small input sizes)
solveTSP :: [[Int]] -> Int -> ([Int], Int)
solveTSP distances n = minimumBy (compare `on` snd) $ map (\path -> (path, pathDistance path)) (permutations [0..n-1])
  where
    -- Calculate total distance of a path
    pathDistance :: [Int] -> Int
    pathDistance path = sum [distances !! (path !! i) !! (path !! (i + 1)) | i <- [0..(length path - 2)]] + distances !! (path !! (length path - 1)) !! (path !! 0)
    
    -- Minimum by comparing distances
    minimumBy :: (a -> a -> Ordering) -> [a] -> a
    minimumBy cmp (x:xs) = foldl' (\m x' -> if cmp x' m == LT then x' else m) x xs
    minimumBy _ [] = error "empty list"

-- Helper to compare distances
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing f x y = compare (f x) (f y)
