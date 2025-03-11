{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints where

import Models (StatusResponse)
import Servant (
  Get,
  (:<|>) (..),
  (:>),
 )
import Servant.API (JSON)

type HealthcheckEndpoints =
  "health" :> "live" :> Get '[JSON] StatusResponse
    :<|> "health" :> "startup" :> Get '[JSON] StatusResponse
    :<|> "health" :> "ready" :> Get '[JSON] StatusResponse

type ReservationEndpoints =
  "api" :> "v1" :> "events" :> Get '[JSON] StatusResponse

type API = HealthcheckEndpoints
