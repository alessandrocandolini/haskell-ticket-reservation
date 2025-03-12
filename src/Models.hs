{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Models where

import Control.Monad ((<=<))
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.UUID (UUID)
import Env (Error, Parser, auto, def, helpDef, nonempty, str, var)
import GHC.Exts (IsString)
import GHC.Generics (Generic)
import Network.HTTP.Types (status500)
import Network.Socket (PortNumber)
import Refined (FromTo, RefineException, Refined, refine)
import Servant.Checked.Exceptions (ErrStatus (toErrStatus))

data AppConfig = AppConfig
  { redisHost :: String
  , redisPort :: PortNumber
  }
  deriving (Eq, Show)

parser :: Parser Error AppConfig
parser =
  AppConfig
    <$> var (str <=< nonempty) "REDIS_HOST" (def "localhost" <> helpDef (helpMessage "Redis host"))
    <*> var (auto <=< nonempty) "REDIS_PORT" (def 6379 <> helpDef (helpMessage "Redis port"))
 where
  helpMessage prefix value = prefix ++ "[default: " ++ show value ++ " ]"

data StatusResponse = Ok deriving (Eq, Show)

instance ToJSON StatusResponse where
  toJSON Ok = object ["status" .= ("ok" :: String)]

newtype ErrorResponse = ErrorResponse String deriving (Generic, Eq, Show)

instance ToJSON ErrorResponse

data DatabaseConnectionError = DatabaseConnectionError deriving (Eq, Show)

instance ToJSON DatabaseConnectionError where
  toJSON DatabaseConnectionError = object ["message" .= ("cannot connect to the database" :: String)]

instance ErrStatus DatabaseConnectionError where
  toErrStatus _ = status500

newtype UserId = UserId UUID
  deriving (Eq, Show)

newtype EventId = EventId String
  deriving (Eq, Show)
  deriving (IsString) via String

newtype SeatId = SeatId (Refined (FromTo 10 1000) Int)
  deriving (Eq, Show)

seatId :: Int -> Either RefineException SeatId
seatId = fmap SeatId . refine

newtype ReservationId = ReservationId UUID
  deriving (Eq, Show)
