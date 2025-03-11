{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints (API) where

import Models (DatabaseConnectionError, StatusResponse)
import Servant (
  Get,
  (:<|>) (..),
  (:>),
 )
import Servant.API (JSON)
import Servant.Checked.Exceptions (Throws)

type HealthcheckEndpoints = "healthcheck" :> Throws DatabaseConnectionError :> Get '[JSON] StatusResponse

type ReservationEndpoints =
  "api" :> "v1" :> "events" :> Get '[JSON] StatusResponse

type API = HealthcheckEndpoints
