{-# LANGUAGE FlexibleContexts #-}

module Database.Task where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import Data.Text (Text)
--------------------------------------------------------------------------------
import Models.Task
import Models.User (UserId)
import Types
--------------------------------------------------------------------------------

class Monad m => DatabaseTask m where
  getTasks :: m [Task]
  getTasks = undefined

  getTask :: TaskId -> m (Maybe Task)
  getTask = undefined

  createTask :: Task -> m Task
  createTask = undefined

  updateTask :: Task -> m ()
  updateTask = undefined

  deleteTask :: TaskId -> m ()
  deleteTask = undefined

  getUserTasks :: UserId -> m [Task]
  getUserTasks = undefined

instance DatabaseTask IO
instance DatabaseTask App
