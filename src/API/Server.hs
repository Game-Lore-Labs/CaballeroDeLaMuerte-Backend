{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server
  ( AppM
  , AppConfig(..)
  , appToHandler
  , app
  , runServer
  ) where

import Network.Wai.Handler.Warp (run)
import Servant
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text)
import GHC.Generics

import API.Routes (api, server)
import API.Middleware (apiMiddleware, customFormatters)

type AppM = ReaderT AppConfig Handler

data AppConfig = AppConfig
  { configPort :: Int
  , configDbConnection :: Text
  , configAIServiceUrl :: Text
  , configLogLevel :: Text
  } deriving (Show, Eq, Generic)

appToHandler :: AppConfig -> AppM a -> Handler a
appToHandler config app = runReaderT app config

app :: AppConfig -> Application
app config =
  serveWithContext api ctx $
    hoistServerWithContext api (Proxy :: Proxy '[]) (appToHandler config) server
  where
    ctx = customFormatters :. EmptyContext

runServer :: AppConfig -> IO ()
runServer config = do
  putStrLn $ "Starting server on port " ++ show (configPort config)
  run (configPort config) (apiMiddleware $ app config)
