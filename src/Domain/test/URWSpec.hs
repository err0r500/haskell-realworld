module URWSpec where

--import Domain.Article
--import Domain.User
--import GHC.IO.Exception
--import Test.Hspec
--import Usecases.UC
--import Implem.URW
--import Control.Concurrent.STM
--
--testUser =
--  Domain.User.User
--    { name = "matth"
--    , email = "userEmail"
--    , password = "userPass"
--    , bio = Domain.User.Bio "userBio"
--    , imageLink = Domain.User.ImageLink "userImgLink"
--    , followNames = ["Name"]
--    , favorites = []
--    }
--
--testArticle = Domain.Article.Article {uuid = "articleID", title = "articleTitle", slug = "articleSlug"}
--
--spec :: Spec
--spec =
--  describe "testing postArticle" $
--  it "should return the user returned" $ do
--    acc <- newTVarIO [testUser]
--    myFunc <- atomically $ userGetByNameConstructor acc
--    myFunc (name testUser) `shouldBe` Right Nothing