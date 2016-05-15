{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Api where

import Servant
import Data.Text

import Types (AppM)

--- API

type ServiceAPI = Get '[PlainText] Text

--- Handlers

serviceAPIHandler :: ServerT ServiceAPI AppM
serviceAPIHandler = return "Hello, world!"
