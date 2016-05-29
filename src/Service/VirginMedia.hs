{-# LANGUAGE OverloadedStrings #-}

module Service.VirginMedia (
      getInternetOptions
) where

import Safe (headMay)
import Network.Wreq
import qualified Network.Wreq.Session as Session
import qualified Data.Text as T
import Control.Lens hiding (children, element, elements)
import Control.Monad (join)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.List (isInfixOf)
import Text.Taggy
import Text.Taggy.Lens
import Data.Char (toLower)
import Text.Regex.Posix
import qualified Data.ByteString.Lazy.Char8 as BS

import Service.Types


-- Internal Types

data AddressOption = AddressOption {
      address :: String
    , addressId :: String
} deriving (Show)


-- Constants

addressOptionsEndpoint :: String
addressOptionsEndpoint = "http://store.virginmedia.com/check-your-postcode-browse"

internetOptionsEndpoint :: String
internetOptionsEndpoint = "http://store.virginmedia.com/serviceability"

maxSpeedRegex :: String
maxSpeedRegex = "var maxSpeed = '([0-9]+)';"


-- API

getInternetOptions :: Query -> IO (Either String [InternetOption])
getInternetOptions query = Session.withSession $ \session -> do
    addressOptions <- getAddressOptionsForPostcode session (postcode query)
    case pickAddress query addressOptions of
        Just addressOption -> Right <$> getInternetOptionsForAddress session (postcode query) addressOption
        Nothing -> return $ Left "Could not find address"

getAddressOptionsForPostcode :: Session.Session -> Postcode -> IO [AddressOption]
getAddressOptionsForPostcode session pc = do
    response <- Session.post session addressOptionsEndpoint ["postcode" := pc]
    let body = decodeUtf8 $ response ^. responseBody
    return $ addresses body
        where
            addresses :: L.Text -> [AddressOption]
            addresses body =
                let
                    elems = body ^.. html
                          . allAttributed (ix "id" . only "addressIdentifier")
                          . allNamed (only "option")
                in
                    mapMaybe mkAddress elems

            mkAddress :: Element -> Maybe AddressOption
            mkAddress e = do
                addressText <- e ^? children . traverse . content
                value <- join $ e ^? element . attr "value"
                if (T.length value) > 0
                    then Just $ AddressOption (T.unpack addressText) (T.unpack value)
                    else Nothing


getInternetOptionsForAddress :: Session.Session -> Postcode -> AddressOption -> IO [InternetOption]
getInternetOptionsForAddress session pc addr = do
    response <- Session.post session internetOptionsEndpoint reqParams
    return $ mkOptions $ BS.unpack (response ^. responseBody)
        where
            reqParams = ["postcode" := pc, "addressIdentifier" := (addressId addr)]

            speed :: [[String]] -> Integer
            speed = read . last . head

            mkOptions :: String -> [InternetOption]
            mkOptions body =
                let
                    speedFragment = body =~ maxSpeedRegex :: [[String]]
                in
                    if null speedFragment
                    then []
                    else [virginMediaOptionFromSpeed (speed speedFragment)]


pickAddress :: Query -> [AddressOption] -> Maybe AddressOption
pickAddress query addressOptions = headMay (filter applyFilters addressOptions)
    where
        applyFilters el = all (\fn -> fn (address el)) [ filterPostcode
                                                       , filterStreet
                                                       , filterBuildingName
                                                       , filterSubBuildingName ]
        dropSpaces = filter (/= ' ')
        lowercase = map toLower

        -- Postcodes must always be present
        filterPostcode x = (dropSpaces (postcode query)) `isInfixOf` (dropSpaces x)

        -- Street names must 'match'
        -- (allow for rudimentary searches by checking query *in* street name)
        filterStreet x = (lowercase (street query)) `isInfixOf` (lowercase x)

        -- If a building name was provided...

        -- Building name in query and in option must match
        filterBuildingName x =
            (lowercase (buildingName query)) `isInfixOf` (lowercase x)

        -- ...and number (assumed to be a flat number) must equal
        -- SubBuildingName
        filterSubBuildingName x =
            (lowercase (streetNumber query)) `isInfixOf` (lowercase x)

virginMediaOptionFromSpeed :: Integer -> InternetOption
virginMediaOptionFromSpeed speed =
    let downSpeed = downloadSpeed speed
        upSpeed = uploadSpeed speed
    in
        InternetOption {
              minDownSpeed = downSpeed
            , maxDownSpeed = downSpeed
            , estDownSpeed = downSpeed
            , minUpSpeed = upSpeed
            , maxUpSpeed = upSpeed
            , estUpSpeed = upSpeed
            , provider = VirginMedia
            , serviceType = FTTC
        }
    where
        uploadSpeed x = round $ (fromIntegral x) * 0.05 * mega
        downloadSpeed x = round $ (fromIntegral x) * mega
