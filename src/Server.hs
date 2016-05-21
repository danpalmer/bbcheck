{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server (
      siteServer
    , makeLogger
    , contextForConfig
    , SiteContext(..)
    , AppM
) where

import Servant
import Network.Wai                    (Application)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Control.Monad.Reader           (runReaderT)
import Control.Monad.Except           (ExceptT)
import Data.Configurator
import Data.Configurator.Types (Config)

import Types (SiteContext(..), AppM)
import Service.Api (ServiceAPI, serviceAPIHandler)

---

type ServerAPI = ServiceAPI

---

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

---

siteServer :: SiteContext -> Application
siteServer ctx = serve serverAPI (readerServer ctx)

readerServer :: SiteContext -> Server ServerAPI
readerServer ctx = enter (readerToExcept ctx) serviceAPIHandler

readerToExcept :: SiteContext -> AppM :~> ExceptT ServantErr IO
readerToExcept ctx = Nat $ \x -> runReaderT x ctx

---

makeLogger :: Config -> IO Middleware
makeLogger cfg = do
    enabled <- lookupDefault True cfg "logging.enabled"
    verbose <- lookupDefault True cfg "logging.verbose"

    if enabled then
        if verbose then
            return logStdoutDev
        else
            return logStdout
    else
        return id

contextForConfig :: Config -> IO (SiteContext)
contextForConfig cfg = do
    return SiteContext {config = cfg}
