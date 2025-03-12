{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints (API) where

import Models (CreateEventRequest, CreateEventResponse, DatabaseConnectionError, EventId, SeatAlreadyReserved, SeatId, Seats, StatusResponse)
import Servant (
  Capture,
  Get,
  JSON,
  NoContent,
  PostAccepted,
  PostCreated,
  ReqBody,
  (:<|>) (..),
  (:>),
 )
import Servant.Checked.Exceptions (Throws)

type HealthcheckEndpoints = "healthcheck" :> Throws DatabaseConnectionError :> Get '[JSON] StatusResponse

type ReservationEndpoints =
  "api" :> "v1" :> "events" :> ReqBody '[JSON] CreateEventRequest :> PostCreated '[JSON] CreateEventResponse
    :<|> "api" :> "v1" :> "events" :> Capture "event_id" EventId :> "seats" :> Get '[JSON] Seats
    :<|> "api" :> "v1" :> "events" :> Capture "event_id" EventId :> "seats" :> Capture "seat_id" SeatId :> "hold" :> Throws SeatAlreadyReserved :> PostAccepted '[JSON] NoContent

type API = HealthcheckEndpoints
