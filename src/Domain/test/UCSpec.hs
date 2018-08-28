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

testArticle = Domain.Article.Article {Domain.Article.id = "articleID", title = "articleTitle"}

spec :: Spec
spec = do
  describe "testing postArticle" $ do
    it "should return the user returned" $
      Usecases.UC.postArticle
        Usecases.UC.Interactor
          { urw = Usecases.UC.UserReadWriter {userGetByName = \x -> Right (Just testUser {name = x})}
          , arw = Usecases.UC.ArticleReadWriter {articleGetBySlug = \_ -> Right Nothing}
          , slugger = Usecases.UC.Slugger {newSlug = \x -> x}
          }
        "matth"
        Domain.Article.Article {} `shouldBe`
      Just Usecases.UC.PostArticleResp {user = Just testUser, article = Nothing}
    it "should return nothing if error" $
      Usecases.UC.postArticle
        Usecases.UC.Interactor {urw = Usecases.UC.UserReadWriter {userGetByName = \x -> Left "woops"}}
        "matth"
        Domain.Article.Article {} `shouldBe`
      Nothing