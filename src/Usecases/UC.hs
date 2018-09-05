{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Usecases.UC
  ( postArticle
  , UserReadWriter(..)
  , ArticleReadWriter(..)
  , Slugger(..)
  , test
  ) where

import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Data.Maybe
import Domain.Article
import Domain.User

class Monad m =>
      UserReadWriter m
  where
  userCreate :: String -> String -> String -> m (Either String Domain.User.User)
  userGetByName :: String -> m (Either String (Maybe Domain.User.User))

class Monad m =>
      ArticleReadWriter m
  where
  articleCreate :: Domain.Article.Article -> m (Either String Domain.Article.Article)
  articleSave :: Domain.Article.Article -> m (Either String Domain.Article.Article)
  articleGetBySlug :: String -> m (Either String (Maybe Domain.Article.Article))

class Monad m =>
      Slugger m
  where
  newSlug :: String -> m (Either String String)

class Monad m =>
      BusinessLogic m
  where
  postArticle :: String -> Domain.Article.Article -> m (Either String (Domain.User.User, Domain.Article.Article))

instance (UserReadWriter m, ArticleReadWriter m, Slugger m, Monad m) => BusinessLogic m where
  postArticle username article = do
    res <-
      runExceptT $ do
        _user <- ExceptT $ userGetByName username
        case _user of
          Nothing -> return (Left "user not found")
          Just user -> do
            slug <- ExceptT $ newSlug (title article)
            articleInStorage <- ExceptT $ articleGetBySlug slug
            case articleInStorage of
              Just _ -> return (Left "article already exists with the same slug")
              Nothing -> do
                newArticle <- EitherT $ articleCreate article {slug = slug}
                return (Right (user, newArticle))
    case res of
      Left err -> return (Left err)
      Right u -> return u




instance Monad m => UserReadWriter (IdentityT m) where
  userCreate _ _ _ = return (Left "oops")
  userGetByName _ = return (Left "oops")

instance Monad m => ArticleReadWriter (IdentityT m) where
  articleCreate _ = return (Left "oops")
  articleSave _ = return (Left "oops")
  articleGetBySlug _ = return (Left "oops")

instance Monad m => Slugger (IdentityT m) where
  newSlug _ = return (Left "oops")

test :: IO ()
test = do
  res <- runIdentityT (postArticle "username" Article {})
  print res