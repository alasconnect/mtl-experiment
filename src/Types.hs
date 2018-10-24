{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.IO.Class
import Servant
--------------------------------------------------------------------------------

data AppContext = AppContext

newtype App a = App { runApp :: ReaderT AppContext Handler a }
  deriving (Functor, Applicative, Monad, MonadReader AppContext, MonadIO)
