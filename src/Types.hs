module Types (AppM, SiteContext(..)) where

import Servant                        (ServantErr)
import Control.Monad.Reader           (ReaderT)
import Control.Monad.Trans.Either     (EitherT)
import Data.Configurator.Types        (Config)


type AppM = ReaderT SiteContext (EitherT ServantErr IO)

data SiteContext = SiteContext
    { config :: Config
    }
