module Domain.Article
  ( Article(..)
  ) where

data Article = Article
  { uuid :: String
  , title :: String
  , slug :: String
  } deriving (Show, Eq)