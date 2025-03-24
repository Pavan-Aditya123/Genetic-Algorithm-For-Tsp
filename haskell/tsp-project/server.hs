{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import System.IO (hSetEncoding, stdout, utf8)
import System.Process (callCommand)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import System.Environment (getArgs)
import Data.Aeson (object, (.=))

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    
    case args of
        ["serve"] -> startServer
        ["run", inputFile, outputFile] -> do
            -- Run the TSP solver directly
            callCommand $ "cabal run tsp-project -- solve " ++ inputFile ++ " " ++ outputFile
        _ -> startServer

startServer :: IO ()
startServer = do
    hSetEncoding stdout utf8  

    putStrLn "✅ Server running at http://localhost:3000 🚀"
    putStrLn "📊 TSP Genetic Algorithm Server"

    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "public")

        get "/" $ file "public/index.html"

        get "/result" $ do
            jsonData <- liftIO $ B.readFile "public/output.json"
            raw jsonData
            
        post "/solve-tsp" $ do
            -- Save the request body to input.json
            reqBody <- body
            liftIO $ B.writeFile "public/input.json" reqBody
            
            -- Fork a thread to run the TSP solver
            _ <- liftIO $ forkIO $ do
                -- Run the solver
                callCommand "cabal run tsp-project -- solve public/input.json public/output.json"
                
            json $ object ["status" .= ("processing" :: String)]
