{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.VirginMedia (
      getInternetOptionsForPostcode
) where

import Safe (headMay)
import Network.Wreq
import qualified Network.Wreq.Session as Session
import qualified Data.Text as T
import Control.Lens hiding (children, element, elements)
import Control.Monad (join)
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.List (isInfixOf)
import Data.List.Lens
import Text.Taggy
import Text.Taggy.Lens
import Data.Char (toLower)
import Text.Regex.Posix
import qualified Data.ByteString.Lazy.Char8 as BS

import Service.Types


-- Internal Types

data AddressOption = AddressOption {
      address :: String
    , value :: String
} deriving (Show)


-- Constants

addressOptionsEndpoint :: String
addressOptionsEndpoint = "http://store.virginmedia.com/check-your-postcode-browse"

internetOptionsEndpoint :: String
internetOptionsEndpoint = "http://store.virginmedia.com/serviceability"

maxSpeedRegex :: String
maxSpeedRegex = "var maxSpeed = '([0-9]+)';"


-- API

getInternetOptionsForPostcode :: String -> String -> IO (Either String [InternetOption])
getInternetOptionsForPostcode pc ad = Session.withSession $ \session -> do
    addressOptions <- getAddressOptionsForPostcode session pc
    case pickAddress ad addressOptions of
        Just addressOption -> Right <$> getInternetOptionsForAddress session pc addressOption
        Nothing -> return $ Left "Could not find address"

getAddressOptionsForPostcode :: Session.Session -> String -> IO ([AddressOption])
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
                    catMaybes $ map address elems

            address :: Element -> Maybe AddressOption
            address elem = do
                addressText <- elem ^? children . traverse . content
                value <- join $ elem ^? element . attr "value"
                if (T.length value) > 0
                    then Just $ AddressOption (T.unpack addressText) (T.unpack value)
                    else Nothing


getInternetOptionsForAddress :: Session.Session -> String -> AddressOption -> IO ([InternetOption])
getInternetOptionsForAddress session pc address = do
    response <- Session.post session internetOptionsEndpoint params
    return $ options $ BS.unpack (response ^. responseBody)
        where
            params = ["postcode" := pc, "addressIdentifier" := (value address)]

            speed :: [[String]] -> Integer
            speed = read . last . head

            options :: String -> [InternetOption]
            options body =
                let
                    speedFragment = body =~ maxSpeedRegex :: [[String]]
                in
                    if null speedFragment
                    then []
                    else [virginMediaOptionFromSpeed (speed speedFragment)]


pickAddress :: String -> [AddressOption] -> Maybe AddressOption
pickAddress ad addressOptions = headMay (filter containsAddress addressOptions)
    where
        containsAddress = (isInfixOf (map toLower ad)) . (map toLower) . address

virginMediaOptionFromSpeed :: Integer -> InternetOption
virginMediaOptionFromSpeed speed = error "undefined"

--toInternetOption :: BTInternetOption -> InternetOption
--toInternetOption opt =
--    case _infinity opt of
--        True -> infinityOption opt
--        False -> dslOption opt
--    where
--        infinityOption o = InternetOption {
--              minDownSpeed = strKbpsToBps $ fromJust $ _CLEAN_MIN_DOWNSPEED o
--            , maxDownSpeed = strKbpsToBps $ fromJust $ _CLEAN_TOP_DOWNSPEED o
--            , estDownSpeed = strKbpsToBps $ fromJust $ _CLEAN_BOTTOM_DOWNSPEED o
--            , minUpSpeed = strKbpsToBps $ fromJust $ _CLEAN_BOTTOM_UPSPEED o
--            , maxUpSpeed = strKbpsToBps $ fromJust $ _CLEAN_TOP_UPSPEED o
--            , estUpSpeed = strKbpsToBps $ fromJust $ _CLEAN_BOTTOM_UPSPEED o
--            , provider = OpenReach
--            , serviceType = FTTC
--        }

--        dslOption o = InternetOption {
--              minDownSpeed = humanStringToBps $ fromJust $ _MinThreshold o
--            , maxDownSpeed = humanStringToBps $ fromJust $ _maxRangeSpeed o
--            , estDownSpeed = humanStringToBps $ fromJust $ _speed o
--            , minUpSpeed = 0
--            , maxUpSpeed = 0
--            , estUpSpeed = 0
--            , provider = OpenReach
--            , serviceType = DSL
--        }

--        -- "10500" -> 10500000
--        strKbpsToBps :: String -> Integer
--        strKbpsToBps = round . (kilo *) . read

--        -- "10.5M" -> 10500000
--        humanStringToBps :: String -> Integer
--        humanStringToBps s =
--            let
--                num = init s
--                unit = case (toLower $ last s) of
--                    'k' -> kilo
--                    'm' -> mega
--                    'g' -> giga
--                    _ -> one
--            in
--                round $ unit * (read num)
