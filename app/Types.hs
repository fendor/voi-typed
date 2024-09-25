{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Servant

data ApiData = ApiData
  { listLengths :: !Int
  }

data MyState = MyState
  { stateVar :: !(TVar (Map Int ApiData))
  , indexVar :: !(TVar Int)
  }

type AppM = ReaderT MyState Handler

type Api = NamedRoutes MyApi

data MyApi mode = MyApi
  { postData ::
      mode :- "api" :> QueryParam "x" Int :> QueryParam "y" Int :> Post '[PlainText] Text
  , getData ::
      mode :- "api" :> Get '[JSON] Double
  }
  deriving (Generic)
