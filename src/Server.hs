{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Server (
      siteServer
    , makeLogger
    , contextForConfig
    , SiteContext(..)
    , AppM
) where

import           Control.Monad.Except                 (ExceptT)
import           Control.Monad.Reader                 (runReaderT)
import           Data.Configurator
import           Data.Configurator.Types              (Config)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant

import           Service.Api                          (ServiceAPI,
                                                       serviceAPIHandler)
import           Types                                (AppM, SiteContext (..))

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

contextForConfig :: Config -> SiteContext
contextForConfig cfg = SiteContext {config = cfg}
