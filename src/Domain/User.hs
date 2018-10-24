{-# LANGUAGE FlexibleInstances #-}

module Domain.User where

--------------------------------------------------------------------------------
import Database.User
import qualified Database.User as U
import Models.User (User, UserId)
import Types
--------------------------------------------------------------------------------

class (Monad m, DatabaseUser m) => DomainUser m where
  getUsers :: m [User]
  getUsers = U.getUsers

  getUser :: UserId -> m (Maybe User)
  getUser = U.getUser

  createUser :: User -> m User
  createUser = U.createUser

  updateUser :: User -> m ()
  updateUser = U.updateUser

  deleteUser :: UserId -> m ()
  deleteUser = U.deleteUser

instance DomainUser IO
instance DomainUser App
