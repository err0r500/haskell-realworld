module Usecases.UC
  ( postArticle
  , PostArticleResp(..)
  , Interactor(..)
  , UserReadWriter(..)
  , ArticleReadWriter(..)
  , Slugger (..)
  ) where

import Data.Maybe
import Domain.Article
import Domain.User

import Control.Exception

data Interactor = Interactor
  { urw :: UserReadWriter
  , arw :: ArticleReadWriter
  , slugger :: Slugger
  }

data UserReadWriter = UserReadWriter
  { userCreate :: String -> String -> String -> (Maybe Domain.User.User, Maybe IOError)
  , userGetByName :: String -> Either String (Maybe Domain.User.User)
  }

data ArticleReadWriter = ArticleReadWriter
  { articleCreate :: Domain.Article.Article -> Either String (Maybe Domain.Article.Article)
  , articleSave :: Domain.Article.Article -> Either String (Maybe Domain.Article.Article)
  , articleGetBySlug :: String -> Either String (Maybe Domain.Article.Article)
  }

newtype Slugger = Slugger{newSlug :: String -> String}

data PostArticleResp = PostArticleResp
  { user :: Maybe Domain.User.User
  , article :: Maybe Domain.Article.Article
  } deriving (Show, Eq)

postArticle :: Interactor -> String -> Domain.Article.Article -> Maybe PostArticleResp
postArticle i username article =
  case userGetByName (urw i) username of
    Left err -> Nothing
    Right user ->
      let slug = newSlug (slugger i) (title article)
       in case articleGetBySlug (arw i) slug of
            Left err -> Nothing
            Right article ->
              case article of
                Just _ -> Nothing
                Nothing -> return PostArticleResp {user = user, article = Nothing}