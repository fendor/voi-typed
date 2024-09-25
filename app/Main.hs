{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Debug.Stub (withGhcDebug)
import GHC.Generics
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant

data ApiData = ApiData
  { responseLength :: !Int
  }

data MyState = MyState
  { stateVar :: TVar (Map Int ApiData)
  , indexVar :: TVar Int
  }

main :: IO ()
main = withGhcDebug application

application :: IO ()
application = do
  indexRef <- newTVarIO 0
  apiDataVar <- newTVarIO Map.empty

  let
    initialState = MyState apiDataVar indexRef
  withStdoutLogger $ \aplogger -> do
    let
      settings = setPort 8085 $ setLogger aplogger defaultSettings
    runSettings settings (app initialState)

app :: MyState -> Application
app initialDb = do
  serve (Proxy @Api) (myApiApp initialDb)

myApiApp :: MyState -> Servant.Server Api
myApiApp initialDb =
  hoistServer (Proxy @Api) (nt initialDb) handler
 where
  nt :: MyState -> AppM a -> Handler a
  nt s x = runReaderT x s

type AppM = ReaderT MyState Handler

type Api = NamedRoutes MyApi

data MyApi mode = MyApi
  { postData ::
      mode
        :- "api"
          :> QueryParam "x" Int
          :> QueryParam "y" Int
          :> Post '[JSON] Text
  }
  deriving (Generic)

handler :: ServerT Api AppM
handler =
  MyApi
    { postData = postHandler
    }

postHandler :: Maybe Int -> Maybe Int -> AppM Text
postHandler (Just x) (Just y) = do
  let
    resultMessage =
      Text.unlines
        [ "Thanks for using the addition service!"
        , ""
        , "We are proud project maintained by memory leaks."
        , "We add your inputs and produce the result:"
        , "x: " <> Text.pack (show x)
        , "y: " <> Text.pack (show y)
        , ""
        , "Result: " <> Text.pack (show $ x + y)
        ]

  db <- asks stateVar
  index <- asks indexVar

  liftIO $ atomically $ do
    n <- readTVar index
    modifyTVar' db (Map.insert n (ApiData $ Text.length resultMessage))
    modifyTVar' index (+ 1)

  pure resultMessage
postHandler _ _ = throwError err404
