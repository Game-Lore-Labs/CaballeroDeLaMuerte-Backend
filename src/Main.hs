{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Data.IORef

-- Application Layer imports
import Application.GameService

-- API Layer imports
import API.Routes

-- | CORS policy for frontend access
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Nothing  -- Allow all origins
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type", "Authorization"]
    }

-- | Main entry point
main :: IO ()
main = do
    -- Initialize server state
    stateRef <- newIORef emptyServerState

    -- Start Scotty server
    scotty 3000 $ do
        -- Enable CORS
        middleware $ cors (const $ Just corsPolicy)

        -- Register routes
        routes stateRef defaultConfig
