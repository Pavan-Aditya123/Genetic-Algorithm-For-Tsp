{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import System.IO (hSetEncoding, stdout, utf8) -- ✅ Add encoding support
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    hSetEncoding stdout utf8  -- ✅ Fix Unicode issue

    liftIO $ putStrLn "✅ Server running at http://localhost:3000 🚀"

    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase "public")

        get "/" $ file "public/index.html"

        get "/result" $ do
            jsonData <- liftIO $ B.readFile "public/output.json"
            raw jsonData
