{-# LANGUAGE TemplateHaskell #-}
module Service.BTSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Data.DeriveTH
import qualified Data.ByteString.Lazy as LBS

import Service.Types

import Service.BT


queryNumber = Query {
    postcode = "E1 2JA"
  , buildingName = ""
  , street = "Cavell Street"
  , streetNumber = "104"
}

queryName = Query {
    postcode = "E1 2JA"
  , buildingName = "Foobar House"
  , street = "Cavell Street"
  , streetNumber = "Flat 1"
}

addressOptionNumber = AddressOption {
      _BuildingName = Just ""
    , _BuildingNumber = Just "104"
    , _County = Just ""
    , _PostTown = ""
    , _Postalcode = "E1 2JA"
    , _SubBuildingName = Just ""
    , _ThoroughfareName = "Cavell Street"
    , _addressId = ""
}

addressOptionName = AddressOption {
      _BuildingName = Just "Foobar House"
    , _BuildingNumber = Just "104"
    , _County = Just ""
    , _PostTown = ""
    , _Postalcode = "E1 2JA"
    , _SubBuildingName = Just "Flat 1"
    , _ThoroughfareName = "Cavell Street"
    , _addressId = ""
}


derive makeArbitrary ''Query
derive makeArbitrary ''AddressOption


spec :: Spec
spec =
    describe "pickAddress" $ do
        it "should return Nothing when no options" $
            (pickAddress queryNumber []) `shouldBe` Nothing

        it "should return Just an option with valid inputs" $ do
            (pickAddress queryNumber [addressOptionNumber]) `shouldBe` (Just addressOptionNumber)
            (pickAddress queryName [addressOptionName]) `shouldBe` (Just addressOptionName)

        it "should return Nothing when postcodes do not match" $ do
            let q = queryNumber {postcode = "NOT A MATCH"}
            (pickAddress q [addressOptionNumber]) `shouldBe` Nothing

        it "should return Nothing when streets do not match" $ do
            let q = queryNumber {street = "NOT A MATCH"}
            (pickAddress q [addressOptionNumber]) `shouldBe` Nothing

        it "should return Just an option with a partial street name" $ do
            let q = queryNumber {street = "Cavell"}
            (pickAddress q [addressOptionNumber]) `shouldBe` (Just addressOptionNumber)

        it "should return Nothing if no building name and the number doesn't match" $ do
            let q = queryNumber {streetNumber = "NOT A MATCH"}
            (pickAddress q [addressOptionNumber]) `shouldBe` Nothing

        it "should return Nothing if no building name and the number doesn't exactly match" $ do
            let q = queryNumber {streetNumber = "10"}
            (pickAddress q [addressOptionNumber]) `shouldBe` Nothing

        it "should return Nothing if flat name does not match" $ do
            let q = queryName {streetNumber = "Flat 2"}
            (pickAddress q [addressOptionName]) `shouldBe` Nothing

        it "should return Just an option if partial flat name match" $ do
            let q = queryName {streetNumber = "1"}
            (pickAddress q [addressOptionName]) `shouldBe` (Just addressOptionName)

        it "should return Nothing if the building name does not match" $ do
            let q = queryName {buildingName = "NOT A MATCH"}
            (pickAddress q [addressOptionName]) `shouldBe` Nothing

        it "should return Just an option if partial building name match" $ do
            let q = queryName {buildingName = "Foobar"}
            (pickAddress q [addressOptionName]) `shouldBe` (Just addressOptionName)

        it "should pick the correct address from Data1.json" $ do
            let q = Query "SW2 1DX" "" "Kellett Road" "1"
            contents <- LBS.readFile "test/Service/BTSpecData1.json"
            let o = fromJust (decode contents) :: [AddressOption]
            (_addressId <$> (pickAddress q o)) `shouldBe` (Just "Gold|A00023971915|WR|1")

        it "should not match options with no street number (pay & display meters)" $ do
            let o = addressOptionNumber {_BuildingNumber = Nothing}
            (pickAddress queryNumber [o]) `shouldBe` Nothing

        it "should pass quickcheck" $ property $
            \(x, y) -> (pickAddress x y) `shouldBe` Nothing
