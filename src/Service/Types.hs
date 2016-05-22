module Service.Types where

-- Core type for available internet options at an address
--   Note: Speeds are in *bits per second*.

data InternetOption = InternetOption {
      minDownSpeed :: Integer
    , maxDownSpeed :: Integer
    , estDownSpeed :: Integer
    , minUpSpeed :: Integer
    , maxUpSpeed :: Integer
    , estUpSpeed :: Integer
    , provider :: Provider
    , serviceType :: ServiceType
} deriving (Show, Eq)

data Provider = OpenReach | VirginMedia | Hyperoptic
    deriving (Show, Eq)

data ServiceType = DSL | FTTC | FTTH
    deriving (Show, Eq)

kilo :: Double
kilo = 1.0e3

mega :: Double
mega = 1.0e6

giga :: Double
giga = 1.0e9

one :: Double
one = 1.0
