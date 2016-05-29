{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.BT (
      getInternetOptions,
      pickAddress,
      AddressOption(..)
) where

import Safe (headMay)
import Network.Wreq
import Control.Lens
import Data.Text (pack)
import Data.Aeson.TH
import Data.Maybe (fromJust, fromMaybe)
import Data.List (isInfixOf)
import Data.Char (toLower)

import Service.Types


-- Internal Types

data AddressOption = AddressOption {
      _BuildingName :: Maybe String
    , _BuildingNumber :: Maybe String
    , _County :: Maybe String
    , _PostTown :: String
    , _Postalcode :: String
    , _SubBuildingName :: Maybe String
    , _ThoroughfareName :: String
    , _addressId :: String
} deriving (Show, Eq)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1 } ''AddressOption)

data AddressOptionResponse = AddressOptionResponse {
      addresses :: [AddressOption]
} deriving (Show, Eq)

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
} deriving (Show)

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

getInternetOptions :: Query -> IO (Either String [InternetOption])
getInternetOptions query = do
    addressOptions <- getAddressOptions (postcode query)
    case pickAddress query addressOptions of
        Just address -> Right <$> getInternetOptionsForAddress address
        Nothing -> return $ Left "Could not find address"

getAddressOptions :: Postcode -> IO [AddressOption]
getAddressOptions pc = do
    response <- asJSON =<< getWith opts addressOptionsEndpoint
    return $ addresses $ response ^. responseBody
        where opts = defaults & param "postcode" .~ [pack pc]
                              & param "format" .~ ["json"]
                              & param "house" .~ [""]


getInternetOptionsForAddress :: AddressOption -> IO [InternetOption]
getInternetOptionsForAddress address = do
    response <- asJSON =<< getWith opts internetOptionsEndpoint
    let btOptions = serviceLineTypes (response ^. responseBody)
    print $ show btOptions
    return $ toInternetOption <$> btOptions
        where opts = defaults & param "addressId" .~ [pack $ _addressId address]
                              & param "format" .~ ["json"]


pickAddress :: Query -> [AddressOption] -> Maybe AddressOption
pickAddress query addressOptions = headMay (filter applyFilters addressOptions)
    where
        applyFilters el = all (\fn -> fn el) [ filterPostcode
                                             , filterStreet
                                             , filterBuildingName
                                             , filterSubBuildingName
                                             , filterBuildingNumber ]

        dropSpaces = filter (/= ' ')
        notNull = not . null

        -- Postcodes must always match
        filterPostcode x = dropSpaces (_Postalcode x) == dropSpaces (postcode query)

        -- Street names must 'match'
        -- (allow for rudimentary searches by checking query *in* street name)
        filterStreet x = (street query) `isInfixOf` (_ThoroughfareName x)

        -- If a building name was provided...

        -- Building name in query and in option must match
        filterBuildingName x =
            let qbn = (buildingName query) in
                case (_BuildingName x) of
                    Nothing -> True
                    Just bn -> (qbn `isInfixOf` bn) || (null qbn)

        -- ...and number (assumed to be a flat number) must equal
        -- SubBuildingName
        filterSubBuildingName x =
            case (_SubBuildingName x) of
                Nothing -> True
                Just sbn -> (streetNumber query) `isInfixOf` sbn || null (buildingName query)

        -- Else, if no building name was provided the number must match
        filterBuildingNumber x =
            case (_BuildingNumber x) of
                Nothing -> True
                Just bn -> ((streetNumber query) == bn) || notNull (buildingName query)


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
              minDownSpeed = humanStringToBps $ minThreshold o
            , maxDownSpeed = humanStringToBps $ maxRangeSpeed o
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

        minThreshold o = fromMaybe (fromJust $ _speed o) (_MinThreshold o)
        maxRangeSpeed o = fromMaybe (fromJust $ _speed o) (_maxRangeSpeed o)
