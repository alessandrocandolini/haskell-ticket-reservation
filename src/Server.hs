{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as B
import Data.Data (Proxy (Proxy))
import Database.Redis (Connection, Reply, checkedConnect, defaultConnectInfo, echo, runRedis)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (
  Get,
  Handler,
  Server,
  err500,
  errBody,
  errHeaders,
  throwError,
  (:<|>) (..),
  (:>),
 )
import Servant.API (JSON)
import Servant.Server (serve)

newtype StatusResponse = StatusResponse
  { status :: String
  }
  deriving (Generic, Eq, Show)

instance ToJSON StatusResponse

newtype ErrorResponse = ErrorResponse
  { message :: String
  }
  deriving (Generic, Eq, Show)

instance ToJSON ErrorResponse

type HealthcheckEndpoints =
  "health" :> "live" :> Get '[JSON] StatusResponse
    :<|> "health" :> "startup" :> Get '[JSON] StatusResponse
    :<|> "health" :> "ready" :> Get '[JSON] StatusResponse

type API = HealthcheckEndpoints

throw500With :: String -> Handler a
throw500With msg =
  throwError $
    err500
      { errBody = encode (ErrorResponse msg)
      , errHeaders = [("Content-Type", "application/json")]
      }

app :: IO Application
app = do
  connection <- checkedConnect defaultConnectInfo
  pure $ buildApp connection

buildApp :: Connection -> Application
buildApp conn = serve api (server conn)
 where
  api :: Proxy API
  api = Proxy

runApp :: IO ()
runApp = app >>= run 8080

startup :: Connection -> IO (Either Reply B.ByteString)
startup connection = runRedis connection (echo "hello")

server :: Connection -> Server API
server connection =
  liveHandler
    :<|> startupHandler connection
    :<|> readyHandler
 where
  ok = StatusResponse "ok"
  liveHandler = pure ok
  readyHandler = pure ok
  startupHandler conn = do
    result <- liftIO $ startup conn
    case result of
      Right _ -> pure (StatusResponse "ok")
      Left _ -> throw500With "Redis connection failed"
