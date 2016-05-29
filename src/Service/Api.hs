{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Api where

import GHC.Generics
import Servant
import Data.Aeson
import Data.Either (rights)
import Control.Monad.IO.Class (liftIO)

import Types (AppM)

import qualified Service.BT as BTUtils
import qualified Service.VirginMedia as VirginMediaUtils

import Service.Types

--- API

type ServiceAPI = "lookup" :> ReqBody '[JSON] Query :> Post '[JSON] LookupResult

--- Model

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
    let resultList = formatResults responses
    let errorList = formatErrors responses
    return $ LookupResult {options = resultList, errors = errorList}
        where
            serviceLookups :: [(Provider, Query -> IO (Either String [InternetOption]))]
            serviceLookups = [ (VirginMedia, VirginMediaUtils.getInternetOptions)
                             , (OpenReach, BTUtils.getInternetOptions)
                             ]

            serviceResponses :: IO [(Provider, Either String [InternetOption])]
            serviceResponses = flip mapM serviceLookups $ \(p, fn) -> do
                optionOrError <- fn query
                return (p, optionOrError)

            formatErrors :: [(Provider, Either String [InternetOption])] -> [LookupError]
            formatErrors responses = [LookupError p e | (p, Left e) <- responses]

            formatResults :: [(Provider, Either String [InternetOption])] -> [InternetOption]
            formatResults = concat . rights . (map snd)
