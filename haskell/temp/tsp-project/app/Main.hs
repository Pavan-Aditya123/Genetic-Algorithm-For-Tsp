{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (permutations, minimumBy)
import Data.Ord (comparing)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B  -- Use Char8 for string conversion

-- Example distance matrix
distanceMatrix :: [[Int]]
distanceMatrix =
    [ [0, 10, 15, 20]
    , [10, 0, 35, 25]
    , [15, 35, 0, 30]
    , [20, 25, 30, 0]
    ]

-- Calculate the total distance of a path
pathDistance :: [[Int]] -> [Int] -> Int
pathDistance matrix path = sum $ zipWith (\a b -> matrix !! a !! b) path (tail path ++ [head path])

-- Find the optimal TSP path
solveTSP :: [[Int]] -> ([Int], Int)
solveTSP matrix =
    let n = length matrix
        cities = [0 .. n - 1]
        allPaths = permutations cities
        bestPath = minimumBy (comparing (pathDistance matrix)) allPaths
    in (bestPath, pathDistance matrix bestPath)

-- Write the result to a JSON file with correct field order
writeJsonOutput :: [Int] -> Int -> IO ()
writeJsonOutput bestPath minDistance = do
    let result = object
            [ "minimum_distance" .= minDistance  -- First field
            , "optimal_path" .= bestPath         -- Second field
            ]
        jsonData = encode result
    B.writeFile "public/output.json" jsonData
    putStrLn "✅ TSP result saved to public/output.json"

-- Main function
main :: IO ()
main = do
    let (bestPath, minDistance) = solveTSP distanceMatrix
    putStrLn "Optimal TSP Path:"
    print bestPath
    putStrLn $ "Minimum Distance: " ++ show minDistance
    writeJsonOutput bestPath minDistance
