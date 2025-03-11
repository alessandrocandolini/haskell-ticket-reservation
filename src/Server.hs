module Server where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import qualified Data.ByteString as B
import Data.Data (Proxy (Proxy))
import Database.Redis (Connection, Reply, checkedConnect, defaultConnectInfo, echo, runRedis)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (
  Handler,
  Server,
  err500,
  errBody,
  errHeaders,
  throwError,
  (:<|>) (..),
 )
import Servant.Server (serve)
import Models
import Endpoints (API)

throw500With :: String -> Handler a
throw500With msg =
  throwError $
    err500
      { errBody = encode (ErrorResponse msg)
      , errHeaders = [("Content-Type", "application/json")]
      }

app :: IO Application
app = do
  connection <- checkedConnect defaultConnectInfo --  checkedConnect incorporates a ping
  pure $ buildApp connection

buildApp :: Connection -> Application
buildApp conn = serve api (handlers conn)
 where
  api :: Proxy API
  api = Proxy

runApp :: IO ()
runApp = app >>= run 8080

startup :: Connection -> IO (Either Reply B.ByteString)
startup connection = runRedis connection (echo "hello")

handlers :: Connection -> Server API
handlers connection =
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
