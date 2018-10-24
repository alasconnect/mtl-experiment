{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import Domain.Task
import Domain.User
import Models.Task (Task)
import Models.User (User, UserId)
import Types
--------------------------------------------------------------------------------

type GetUsersApi
  = Get '[JSON] [User]

type GetUserApi
  =  Capture "user_id" UserId
  :> Get '[JSON] (Maybe User)

type CreateUserApi
  =  ReqBody '[JSON] User
  :> Post '[JSON] User

type UpdateUserApi
  =  ReqBody '[JSON] User
  :> Put '[JSON] NoContent

type DeleteUserApi
  =  Capture "user_id" UserId
  :> Delete '[JSON] NoContent

type GetUserTasksApi
  =  Capture "user_id" UserId
  :> Get '[JSON] [Task]

type GetUserAndTasksApi
  =  "tasks"
  :> Capture "user_id" UserId
  :> Get '[JSON] (Maybe User, [Task])

type UserApi
  =  "api"
  :> "v1"
  :> "users"
  :> (    GetUsersApi
     :<|> GetUserApi
     :<|> CreateUserApi
     :<|> UpdateUserApi
     :<|> DeleteUserApi
     :<|> GetUserTasksApi
     :<|> GetUserAndTasksApi
     )

usersApi :: Proxy UserApi
usersApi = Proxy

usersServer :: (MonadIO m, MonadReader AppContext m, DomainUser m, DomainTask m)
  => ServerT UserApi m
usersServer =
  usersGet
    :<|> userGet
    :<|> userCreate
    :<|> userUpdate
    :<|> userDelete
    :<|> userTasks
    :<|> userAndTasks

usersGet :: (MonadIO m, DomainUser m) => m [User]
usersGet = getUsers

userGet :: (MonadIO m, DomainUser m) => UserId -> m (Maybe User)
userGet = getUser

userCreate :: (MonadIO m, DomainUser m) => User -> m User
userCreate = createUser

userUpdate :: (MonadIO m, DomainUser m) => User -> m NoContent
userUpdate u = updateUser u >> return NoContent

userDelete :: (MonadIO m, DomainUser m) => UserId -> m NoContent
userDelete uid = deleteUser uid >> return NoContent

userTasks :: (MonadIO m, DomainTask m) => UserId -> m [Task]
userTasks = getUserTasks

userAndTasks :: (MonadIO m, MonadReader AppContext m, DomainTask m, DomainUser m)
  => UserId -> m (Maybe User, [Task])
userAndTasks uid = do
  ctx <- ask
  u <- getUser uid
  ts <- getUserTasks uid
  return (u, ts)
