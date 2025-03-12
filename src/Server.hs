{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server (runApp, appWithConfig) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data (Proxy (Proxy))
import Database.Redis (ConnectInfo (connectHost, connectPort), Connection, PortID (PortNumber), connect, defaultConnectInfo)
import Endpoints (API)
import qualified Env
import Models (
  AppConfig (redisHost, redisPort),
  DatabaseConnectionError (..),
  StatusResponse (..),
  parser,
 )
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import RedisClient (checkedConnection)
import Servant (
  Handler,
  ServerT,
 )
import Servant.Checked.Exceptions (Envelope, pureErrEnvelope, pureSuccEnvelope)
import Servant.Server (serve)

runApp :: IO ()
runApp = do
  config <- Env.parse (Env.header "Parse Redis Config") parser
  app <- appWithConfig config
  run 8080 app

appWithConfig :: AppConfig -> IO Application
appWithConfig config = do
  let connectInfo =
        defaultConnectInfo
          { connectHost = redisHost config
          , connectPort = PortNumber (redisPort config)
          }
  connection <- connect connectInfo
  pure $ buildApp connection

buildApp :: Connection -> Application
buildApp conn = serve api (serverHandlers conn)
 where
  api :: Proxy API
  api = Proxy

healthcheckHandler :: Connection -> Handler (Envelope '[DatabaseConnectionError] StatusResponse)
healthcheckHandler connection = do
  result <- liftIO $ checkedConnection connection
  case result of
    Right _ -> pureSuccEnvelope Ok
    Left e -> pureErrEnvelope DatabaseConnectionError

serverHandlers :: Connection -> ServerT API Handler
serverHandlers = healthcheckHandler
