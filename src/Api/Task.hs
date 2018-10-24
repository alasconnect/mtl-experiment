{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Task where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import Domain.Task
import Models.Task (Task, TaskId)
--------------------------------------------------------------------------------

type GetTasksApi
  = Get '[JSON] [Task]

type GetTaskApi
  =  Capture "task_id" TaskId
  :> Get '[JSON] (Maybe Task)

type CreateTaskApi
  =  ReqBody '[JSON] Task
  :> Post '[JSON] Task

type UpdateTaskApi
  =  ReqBody '[JSON] Task
  :> Put '[JSON] NoContent

type DeleteTaskApi
  =  Capture "task_id" TaskId
  :> Delete '[JSON] NoContent

type TaskApi
  =  "api"
  :> "v1"
  :> "tasks"
  :> (    GetTasksApi
     :<|> GetTaskApi
     :<|> CreateTaskApi
     :<|> UpdateTaskApi
     :<|> DeleteTaskApi
     )

tasksApi :: Proxy TaskApi
tasksApi = Proxy

tasksServer :: (MonadIO m, DomainTask m) => ServerT TaskApi m
tasksServer = tasksGet :<|> taskGet :<|> taskCreate :<|> taskUpdate :<|> taskDelete

tasksGet :: (MonadIO m, DomainTask m) => m [Task]
tasksGet = getTasks

taskGet :: (MonadIO m, DomainTask m) => TaskId -> m (Maybe Task)
taskGet = getTask

taskCreate :: (MonadIO m, DomainTask m) => Task -> m Task
taskCreate = createTask

taskUpdate :: (MonadIO m, DomainTask m) => Task -> m NoContent
taskUpdate u = updateTask u >> return NoContent

taskDelete :: (MonadIO m, DomainTask m) => TaskId -> m NoContent
taskDelete uid = deleteTask uid >> return NoContent
