module Service.BTSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Service.Types

import Service.BT


query = Query {
    postcode = "E1 2JA"
  , buildingName = ""
  , street = "Cavell Street"
  , streetNumber = "104"
}


spec :: Spec
spec = do
    describe "toInternetOption" $ do
        it "should return Nothing when no options" $ do
            (pickAddress query []) `shouldBe` Nothing
