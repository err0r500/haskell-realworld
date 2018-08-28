module Domain.User
  ( User(..)
  , Bio(..)
  , ImageLink(..)
  , follows
  , updateFollowees
  , updateUser
  ) where

import qualified Domain.Article

data User = User
  { name :: Name
  , email :: Email
  , password :: Password
  , bio :: Bio
  , imageLink :: ImageLink
  , followNames :: [Name]
  , favorites :: [Domain.Article.Article]
  } deriving (Show, Eq)

type Name = String

type Email = String

type Password = String

newtype Bio =
  Bio String
  deriving (Show, Eq)

newtype ImageLink =
  ImageLink String
  deriving (Show, Eq)

follows :: User -> Name -> Bool
follows user userName = userName `elem` followNames user

updateFollowees :: User -> Bool -> Name -> User
updateFollowees user append followName =
  if append
    then user {followNames = followNames user ++ [followName]}
    else user {followNames = filter (/= followName) (followNames user)}

updateUser :: (UserUpdater u) => User -> u -> User
updateUser = update

class UserUpdater a where
  update :: User -> a -> User

instance UserUpdater Bio where
  update user newBio = user {bio = newBio}

instance UserUpdater ImageLink where
  update user newImgLink = user {imageLink = newImgLink}

