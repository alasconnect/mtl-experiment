{-# LANGUAGE FlexibleInstances #-}

module Domain.Task where

--------------------------------------------------------------------------------
import Database.Task
import qualified Database.Task as U
import Models.Task (Task, TaskId)
import Models.User (UserId)
import Types
--------------------------------------------------------------------------------

class (Monad m, DatabaseTask m) => DomainTask m where
  getTasks :: m [Task]
  getTasks = U.getTasks

  getTask :: TaskId -> m (Maybe Task)
  getTask = U.getTask

  createTask :: Task -> m Task
  createTask = U.createTask

  updateTask :: Task -> m ()
  updateTask = U.updateTask

  deleteTask :: TaskId -> m ()
  deleteTask = U.deleteTask

  getUserTasks :: UserId -> m [Task]
  getUserTasks = U.getUserTasks

instance DomainTask IO
instance DomainTask App
