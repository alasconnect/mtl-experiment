{-# LANGUAGE FlexibleInstances #-}

module Database.User where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import Data.Text (Text)
--------------------------------------------------------------------------------
import Models.User
import Types
--------------------------------------------------------------------------------

class Monad m => DatabaseUser m where
  getUsers :: m [User]
  getUsers = undefined

  getUser :: UserId -> m (Maybe User)
  getUser = undefined

  createUser :: User -> m User
  createUser = undefined

  updateUser :: User -> m ()
  updateUser = undefined

  deleteUser :: UserId -> m ()
  deleteUser = undefined

instance DatabaseUser IO
instance DatabaseUser App
