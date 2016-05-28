{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Api where

import GHC.Generics
import Servant
import Data.Aeson
import Data.Either (rights)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Types (AppM)

import qualified Service.BT as BTUtils
import qualified Service.VirginMedia as VirginMediaUtils

import Service.Types

--- API

type ServiceAPI = "lookup" :> ReqBody '[JSON] Query :> Post '[JSON] LookupResult

--- Model

data Query = Query {
      postcode :: String
    , address :: String
} deriving (Eq, Show, Generic)

instance FromJSON Query

data LookupError = LookupError {
      provider :: Provider
    , message :: String
} deriving (Generic)

instance ToJSON LookupError

data LookupResult = LookupResult {
      options :: [InternetOption]
    , errors :: [LookupError]
} deriving (Generic)

instance ToJSON LookupResult

--- Handlers

serviceAPIHandler :: ServerT ServiceAPI AppM
serviceAPIHandler = lookupHandler

lookupHandler :: Query -> AppM LookupResult
lookupHandler query = do
    responses <- liftIO serviceResponses
    let results = formatResults responses
    let errors = formatErrors responses
    return $ LookupResult {options = results, errors = errors}
        where
            serviceLookups :: [(Provider, String -> String -> IO (Either String [InternetOption]))]
            serviceLookups = [ (VirginMedia, VirginMediaUtils.getInternetOptionsForPostcode)
                             , (OpenReach, BTUtils.getInternetOptionsForPostcode)
                             ]

            serviceResponses :: IO [(Provider, Either String [InternetOption])]
            serviceResponses = flip mapM serviceLookups $ \(p, fn) -> do
                optionOrError <- fn (postcode query) (address query)
                return (p, optionOrError)

            formatErrors :: [(Provider, Either String [InternetOption])] -> [LookupError]
            formatErrors responses = [LookupError p e | (p, Left e) <- responses]

            formatResults :: [(Provider, Either String [InternetOption])] -> [InternetOption]
            formatResults = concat . rights . (map snd)
