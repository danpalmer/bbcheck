{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.BT (
      getInternetOptionsForPostcode
) where

import Safe (headMay)
import Network.Wreq
import Control.Lens
import Data.Text (pack)
import Data.Aeson.TH
import Data.Maybe (isNothing, fromJust)
import Data.List.Split (splitOn)
import Data.Char (toLower)

import Service.Types


-- Internal Types

data AddressOption = AddressOption {
      _BuildingName :: Maybe String
    , _BuildingNumber :: String
    , _County :: Maybe String
    , _PostTown :: String
    , _Postalcode :: String
    , _SubBuildingName :: Maybe String
    , _ThoroughfareName :: String
    , _addressId :: String
} deriving (Show)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1 } ''AddressOption)

data AddressOptionResponse = AddressOptionResponse {
      addresses :: [AddressOption]
    , house :: String
    , postcode :: String
} deriving (Show)

$(deriveJSON defaultOptions ''AddressOptionResponse)

data BTInternetOption = BTInternetOption {
      _infinity :: Bool
    , _exchangeState :: String
    , _readyDate :: String
    , _AvailabilityFlag :: Maybe String

    , _CLEAN_BOTTOM_DOWNSPEED :: Maybe String
    , _CLEAN_BOTTOM_UPSPEED :: Maybe String
    , _CLEAN_MIN_DOWNSPEED :: Maybe String
    , _CLEAN_TOP_DOWNSPEED :: Maybe String
    , _CLEAN_TOP_UPSPEED :: Maybe String

    , _maxRangeSpeed :: Maybe String
    , _minRangeSpeed :: Maybe String
    , _MinThreshold :: Maybe String
    , _speed :: Maybe String
}

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1 } ''BTInternetOption)

data BTInternetOptionsResponse = BTInternetOptionsResponse {
      serviceLineTypes :: [BTInternetOption]
      --exchangeName :: String
}

$(deriveJSON defaultOptions ''BTInternetOptionsResponse)


-- Constants

addressOptionsEndpoint :: String
addressOptionsEndpoint = "https://www.productsandservices.bt.com/consumerProducts/v1/addressSearch.do"

internetOptionsEndpoint :: String
internetOptionsEndpoint = "https://www.productsandservices.bt.com/consumerProducts/v1/productAvailability.do"


-- API

getInternetOptionsForPostcode :: String -> String -> IO (Either String [InternetOption])
getInternetOptionsForPostcode pc address = do
    addressOptions <- getAddressOptionsForPostcode pc
    case pickAddress pc address addressOptions of
        Just address -> Right <$> getInternetOptionsForAddress address
        Nothing -> return $ Left "Could not find address"

getAddressOptionsForPostcode :: String -> IO ([AddressOption])
getAddressOptionsForPostcode pc = do
    response <- asJSON =<< getWith opts addressOptionsEndpoint
    return $ addresses $ response ^. responseBody
        where opts = defaults & param "postcode" .~ [pack pc]
                              & param "format" .~ ["json"]
                              & param "house" .~ [""]


getInternetOptionsForAddress :: AddressOption -> IO ([InternetOption])
getInternetOptionsForAddress address = do
    response <- asJSON =<< getWith opts internetOptionsEndpoint
    let btOptions = serviceLineTypes (response ^. responseBody)
    return $ toInternetOption <$> btOptions
        where opts = defaults & param "addressId" .~ [pack $ _addressId address]
                              & param "format" .~ ["json"]


pickAddress :: String -> String -> [AddressOption] -> Maybe AddressOption
pickAddress pc address addressOptions = headMay (filter applyFilters addressOptions)
    where
        applyFilters el = all (\fn -> fn el) [ filterPostcode
                                             , filterSubBuildingName
                                             , filterBuildingName
                                             , filterBuildingNumber ]

        dropSpaces = filter (/= ' ')

        -- Hard filter on matching postcode
        filterPostcode x =
            dropSpaces (_Postalcode x) == dropSpaces pc

        -- Hard filter on sub building name if specified
        filterSubBuildingName x =
            case (_SubBuildingName x) of
                Nothing -> True
                Just sbn -> sbn == address

        -- Hard filter on building name if specified and sub name not specified
        filterBuildingName x =
            case (_BuildingName x) of
                Nothing -> True
                Just bn -> bn == address || filterSubBuildingName x

        -- Hard filter on building number if more specific value unspecified
        filterBuildingNumber x =
            if isNothing (_SubBuildingName x)
                then if isNothing (_BuildingName x)
                    then _BuildingNumber x == head (splitOn " " address)
                else True
            else True

toInternetOption :: BTInternetOption -> InternetOption
toInternetOption opt =
    case _infinity opt of
        True -> infinityOption opt
        False -> dslOption opt
    where
        infinityOption o = InternetOption {
              minDownSpeed = strKbpsToBps $ fromJust $ _CLEAN_MIN_DOWNSPEED o
            , maxDownSpeed = strKbpsToBps $ fromJust $ _CLEAN_TOP_DOWNSPEED o
            , estDownSpeed = strKbpsToBps $ fromJust $ _CLEAN_BOTTOM_DOWNSPEED o
            , minUpSpeed = strKbpsToBps $ fromJust $ _CLEAN_BOTTOM_UPSPEED o
            , maxUpSpeed = strKbpsToBps $ fromJust $ _CLEAN_TOP_UPSPEED o
            , estUpSpeed = strKbpsToBps $ fromJust $ _CLEAN_BOTTOM_UPSPEED o
            , provider = OpenReach
            , serviceType = FTTC
        }

        dslOption o = InternetOption {
              minDownSpeed = humanStringToBps $ fromJust $ _MinThreshold o
            , maxDownSpeed = humanStringToBps $ fromJust $ _maxRangeSpeed o
            , estDownSpeed = humanStringToBps $ fromJust $ _speed o
            , minUpSpeed = 0
            , maxUpSpeed = 0
            , estUpSpeed = 0
            , provider = OpenReach
            , serviceType = DSL
        }

        -- "10500" -> 10500000
        strKbpsToBps :: String -> Integer
        strKbpsToBps = round . (kilo *) . read

        -- "10.5M" -> 10500000
        humanStringToBps :: String -> Integer
        humanStringToBps s =
            let
                num = init s
                unit = case (toLower $ last s) of
                    'k' -> kilo
                    'm' -> mega
                    'g' -> giga
                    _ -> one
            in
                round $ unit * (read num)
