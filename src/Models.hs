{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Models where

import Control.Monad ((<=<))
import Data.Aeson (ToJSON (toJSON), object, (.=), FromJSON)
import Data.UUID (UUID)
import Env (Error, Parser, auto, def, helpDef, nonempty, str, var)
import GHC.Exts (IsString)
import GHC.Generics (Generic)
import Network.HTTP.Types (status500, status409)
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

newtype MaximumNumberOfSeats = MaximumNumberOfSeats (Refined (FromTo 10 1000) Int)
newtype SeatId = SeatId Int
  deriving (Eq, Show)
  deriving (Num, ToJSON, FromJSON) via Int

newtype ReservationId = ReservationId UUID
  deriving (Eq, Show)

data CreateEventRequest = CreateEventRequest
  { eventName :: String
  , maximumNumberOfSeats :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON CreateEventRequest

newtype CreateEventResponse = CreateEventResponse
  { eventId :: EventId
  }
  deriving (Eq, Show, Generic)

instance ToJSON CreateEventRequest

newtype Seats = Seats {
  availableSeats :: [SeatId] } deriving (Eq, Show, Generic)

instance ToJSON Seats

data SeatAlreadyReserved = SeatAlreadyReserved deriving (Eq, Show)

instance ToJSON SeatAlreadyReserved where
  toJSON SeatAlreadyReserved = object ["message" .= ("seat already reserved" :: String)]

instance ErrStatus SeatAlreadyReserved where
  toErrStatus SeatAlreadyReserved = status409
