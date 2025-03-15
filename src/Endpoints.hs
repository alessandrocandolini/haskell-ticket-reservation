{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints (API) where

import Models (CreateEventRequest, CreateEventResponse, DatabaseConnectionError, EventId, HoldRequest, ReservationRequest, SeatCannotBeHeld, SeatCannotBeReserved, SeatId, Seats, StatusResponse)
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
    :<|> "api" :> "v1" :> "events" :> Capture "event_id" EventId :> "seats" :> Get '[JSON] Seats -- TODO make it paginated
    :<|> "api" :> "v1" :> "events" :> Capture "event_id" EventId :> "holds" :> ReqBody '[JSON] HoldRequest :> Throws SeatCannotBeHeld :> PostAccepted '[JSON] NoContent
    :<|> "api" :> "v1" :> "events" :> Capture "event_id" EventId :> "reservations" :> ReqBody '[JSON] ReservationRequest :> Throws SeatCannotBeReserved :> PostAccepted '[JSON] NoContent

type API = HealthcheckEndpoints
