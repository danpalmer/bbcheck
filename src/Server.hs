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
import Control.Monad.Trans.Either     (EitherT)
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
readerServer ctx = enter (readerToEither ctx) serviceAPIHandler

readerToEither :: SiteContext -> AppM :~> EitherT ServantErr IO
readerToEither ctx = Nat $ \x -> runReaderT x ctx

---

makeLogger :: Config -> IO Middleware
makeLogger config = do
    enabled <- lookupDefault True config "logging.enabled"
    verbose <- lookupDefault True config "logging.verbose"

    if enabled then
        if verbose then
            return logStdoutDev
        else
            return logStdout
    else
        return id

contextForConfig :: Config -> IO (SiteContext)
contextForConfig config = do
    return SiteContext {
            config = config
        }
