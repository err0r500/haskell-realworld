module UCSpec where

import Domain.Article
import Domain.User
import GHC.IO.Exception
import Test.Hspec
import Usecases.UC

testUser =
  Domain.User.User
    { name = "matth"
    , email = "userEmail"
    , password = "userPass"
    , bio = Domain.User.Bio "userBio"
    , imageLink = Domain.User.ImageLink "userImgLink"
    , followNames = ["Name"]
    , favorites = []
    }

testArticle = Domain.Article.Article {uuid = "articleID", title = "articleTitle", slug = "articleSlug"}

spec :: Spec
spec =
  describe "testing postArticle" $ do
    it "should return the user returned" $
      Usecases.UC.postArticle
        Usecases.UC.Interactor
          { urw = Usecases.UC.UserReadWriter {userGetByName = \x -> Right (Just testUser {name = x})}
          , arw =
              Usecases.UC.ArticleReadWriter
                {articleGetBySlug = \_ -> Right Nothing, articleCreate = \_ -> Right (Just testArticle)}
          , slugger = Usecases.UC.Slugger {newSlug = id}
          }
        "matth"
        Domain.Article.Article {} `shouldBe`
      Right (testUser, testArticle)
    it "should return nothing if error" $
      Usecases.UC.postArticle
        Usecases.UC.Interactor {urw = Usecases.UC.UserReadWriter {userGetByName = \x -> Left "woops"}}
        "matth"
        Domain.Article.Article {} `shouldBe`
      Left "woops"