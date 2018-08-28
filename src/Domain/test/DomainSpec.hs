module DomainSpec where

import qualified Domain.User (Bio(..), ImageLink(..), User(..), followNames, follows, updateFollowees, updateUser)
import Test.Hspec

spec :: Spec
spec = do
  describe "testing follows" $ do
    it "return true if in list" $
      Domain.User.follows (Domain.User.User {Domain.User.followNames = ["bla"]}) "bla" `shouldBe` True
    it "empty string and empty list -> False" $
      Domain.User.follows (Domain.User.User {Domain.User.followNames = []}) "" `shouldBe` False
    it "exact string match -> False" $
      Domain.User.follows (Domain.User.User {Domain.User.followNames = ["blabla"]}) "bla" `shouldBe` False
  describe "append followee" $ do
    it "should append" $
      Domain.User.followNames
        (Domain.User.updateFollowees Domain.User.User {Domain.User.followNames = ["bla"]} True "bpbp") `shouldBe`
      ["bla", "bpbp"]
    it "should remove" $
      Domain.User.followNames
        (Domain.User.updateFollowees (Domain.User.User {Domain.User.followNames = ["bla", "bpbp"]}) False "bpbp") `shouldBe`
      ["bla"]
  describe "update user" $
    it "should update bio" $
    Domain.User.bio
      (Domain.User.updateUser Domain.User.User {Domain.User.bio = Domain.User.Bio "bla"} (Domain.User.Bio "heyhey")) `shouldBe`
    Domain.User.Bio "heyhey"