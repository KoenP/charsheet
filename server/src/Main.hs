{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Data.ByteString.Lazy.Char8 (pack)

-- Define the application
app :: Application
app req respond = do
    let response = case pathInfo req of
            [] -> responseLBS status200 [("Content-Type", "text/plain")] (pack "Home Page")
            ["hello"] -> responseLBS status200 [("Content-Type", "text/plain")] (pack "Hello, World!")
            ["goodbye"] -> responseLBS status200 [("Content-Type", "text/plain")] (pack "Goodbye, World!")
            _ -> responseLBS status404 [("Content-Type", "text/plain")] (pack "404 - Not Found")
    respond response

-- Main function to run the server
main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    run 8080 app
