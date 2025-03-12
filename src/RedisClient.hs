module RedisClient where
import Database.Redis (Connection, echo, runRedis)
import Control.Exception (SomeException, try)
import Control.Monad (void)

checkedConnection :: Connection -> IO (Either SomeException ())
checkedConnection = try . void . flip runRedis (echo "hello")

