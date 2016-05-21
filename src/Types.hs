module Types (AppM, SiteContext(..)) where

import Servant                        (ServantErr)
import Control.Monad.Reader           (ReaderT)
import Control.Monad.Except           (ExceptT)
import Data.Configurator.Types        (Config)


type AppM = ReaderT SiteContext (ExceptT ServantErr IO)

data SiteContext = SiteContext
    { config :: Config
    }
