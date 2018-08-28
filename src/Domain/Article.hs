module Domain.Article
  ( Article(..)
  ) where

data Article = Article
  { id :: String
  , title :: String
  } deriving (Show, Eq)