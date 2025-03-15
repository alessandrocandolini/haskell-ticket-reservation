{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.UUID (UUID)
import Env (Error, Parser, auto, def, helpDef, nonempty, str, var)
import GHC.Generics (Generic)
import Network.HTTP.Types (status404, status409, status500)
import Network.Socket (PortNumber)
import Refined (FromTo, Refined)
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

newtype EventId = EventId UUID
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via UUID

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

instance ToJSON CreateEventResponse

newtype Seats = Seats
  { availableSeats :: [SeatId]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Seats

newtype Ttl = Ttl {ttlInSeconds :: Int} deriving (Eq, Show, Num, Ord) via Int

data SeatCannotBeHeld = SeatDoesNotExist SeatId | SeatOnHold SeatId UserId | SeatReserved SeatId UserId deriving (Eq, Show)
data SeatCannotBeReserved = SeatNotHeld SeatId | HoldFailure SeatCannotBeHeld deriving (Eq,Show)

instance ToJSON SeatCannotBeHeld where
  toJSON (SeatDoesNotExist seatId) = object ["message" .= ("seat " ++ show seatId ++ " does not exist" :: String)]
  toJSON (SeatOnHold seatId userId) = object ["message" .= (("seat " ++ show seatId ++ " temporarily held by user " ++ show userId) :: String)]
  toJSON (SeatReserved seatId userId) = object ["message" .= (("seat " ++ show seatId ++ " reserved by user " ++ show userId) :: String)] -- IMPORTANT: we only expose this for troubleshooting, in production we should never ever expose publicly who reserved/held a seat

instance ErrStatus SeatCannotBeHeld where
  toErrStatus (SeatDoesNotExist _) = status404
  toErrStatus (SeatOnHold _ _) = status409
  toErrStatus (SeatReserved _ _) = status409

instance ToJSON SeatCannotBeReserved where
  toJSON (SeatNotHeld seatId) = object ["message" .= ("seat " ++ show seatId ++ " is not held by the user" :: String)]
  toJSON (HoldFailure e) = toJSON e

instance ErrStatus SeatCannotBeReserved where
  toErrStatus (SeatNotHeld _) = status409
  toErrStatus (HoldFailure e) = toErrStatus e

newtype HoldRequest = HoldRequest {
   seatId :: SeatId } deriving (Eq,Show,Generic)

instance FromJSON HoldRequest

newtype ReservationRequest = ReservationRequest {
   seatId :: SeatId } deriving (Eq,Show,Generic)

instance FromJSON ReservationRequest
