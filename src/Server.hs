{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import qualified Env
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import Data.Data (Proxy (Proxy))
import Database.Redis (ConnectInfo (connectHost, connectPort), Connection, PortID (PortNumber), Reply, checkedConnect, defaultConnectInfo, echo, runRedis, connect)
import Endpoints (API)
import Models
    ( DatabaseConnectionError(..),
      StatusResponse(..),
      ApplicationConfig(redisPort, redisHost),
      applicationConfigEnvParser )
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (
  Handler,
  ServerT,
 )
import Servant.Checked.Exceptions (Envelope, pureErrEnvelope, pureSuccEnvelope)
import Servant.Server (serve)
import Control.Exception (try, SomeException)

app :: IO Application
app = do
  config <- Env.parse (Env.header "Parse Redis Config") applicationConfigEnvParser
  appWithConfig config

appWithConfig :: ApplicationConfig -> IO Application
appWithConfig config = do
  connection <- connect defaultConnectInfo{connectHost = redisHost config, connectPort = PortNumber (redisPort config)}
  pure $ buildApp connection

buildApp :: Connection -> Application
buildApp conn = serve api (handlers conn)
 where
  api :: Proxy API
  api = Proxy

runApp :: IO ()
runApp = app >>= run 8080

startup :: Connection -> IO (Either SomeException (Either Reply B.ByteString))
startup connection = try $ runRedis connection (echo "hello")

healthcheckHandler :: Connection -> Handler (Envelope '[DatabaseConnectionError] StatusResponse)
healthcheckHandler conn = do
  result <- liftIO $ startup conn
  case result of
    Right (Right _) -> pureSuccEnvelope Ok
    _ -> pureErrEnvelope DatabaseConnectionError

handlers :: Connection -> ServerT API Handler
handlers = healthcheckHandler
