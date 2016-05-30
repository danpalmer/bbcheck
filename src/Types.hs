module Types (AppM, SiteContext(..)) where

import           Control.Monad.Except    (ExceptT)
import           Control.Monad.Reader    (ReaderT)
import           Data.Configurator.Types (Config)
import           Servant                 (ServantErr)


type AppM = ReaderT SiteContext (ExceptT ServantErr IO)

data SiteContext = SiteContext
    { config :: Config
    }
