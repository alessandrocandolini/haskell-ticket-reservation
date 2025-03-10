{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Reservation where

import Data.UUID (UUID)
import GHC.Exts (IsString)
import Refined (FromTo, RefineException, Refined, refine)

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
