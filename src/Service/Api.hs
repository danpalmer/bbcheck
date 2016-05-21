{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Api where

import Servant
import Data.Text
import qualified Data.Text as T
import Control.Monad.IO.Class

import Types (AppM)

import Service.BT

--- API

type ServiceAPI = "lookup" :> ReqBody '[FormUrlEncoded] Query :> Post '[PlainText] Text

--- Model

data Query = Query {
      postcode :: Text
    , address :: Text
    } deriving (Eq, Show)

instance FromFormUrlEncoded Query where
    fromFormUrlEncoded inputs =
        Query
            <$> field "postcode"
            <*> field "address"
        where
            field label = case lookup label inputs of
                Nothing -> Left $ "Field " ++ (unpack label) ++ " not found"
                Just value -> validate label value
            validate label value = case (T.null value) of
                True -> Left $ "Field " ++ (unpack label) ++ " must not be empty"
                False -> Right value

--- Handlers

serviceAPIHandler :: ServerT ServiceAPI AppM
serviceAPIHandler = lookupHandler

lookupHandler :: Query -> AppM Text
lookupHandler query = do
    liftIO $ putStrLn $ show $ query
    return "Hello, world!"
