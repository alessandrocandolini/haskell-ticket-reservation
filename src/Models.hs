{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Models where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import GHC.Exts (IsString)
import Refined (FromTo, RefineException, Refined, refine)

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
