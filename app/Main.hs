{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Main where

import GHC.Debug.Stub (withGhcDebug)

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant

import Types

main :: IO ()
main = withGhcDebug application

application :: IO ()
application = do
  putStrLn "Server is ready!"
  indexRef <- newTVarIO 0
  apiDataVar <- newTVarIO Map.empty

  let
    initialState = MyState apiDataVar indexRef
  withStdoutLogger $ \_aplogger -> do
    let
      settings = setPort 8085 defaultSettings
    -- \$ setLogger aplogger defaultSettings
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

handler :: ServerT Api AppM
handler =
  MyApi
    { postData = postHandler
    , getData = getHandler
    }

postHandler :: Maybe Int -> Maybe Int -> AppM Text
postHandler (Just x) (Just y) = do
  let
    ls = [x .. y]
    s = sum ls

  db <- asks stateVar
  index <- asks indexVar

  liftIO $ atomically $ do
    n <- readTVar index
    modifyTVar' db (Map.insert n (ApiData $ length ls))
    modifyTVar' index (+ 1)

  pure $
    Text.unlines
      [ "Thanks for using the summation service!"
      , ""
      , "We are proud project maintained by memory leaks."
      , ""
      , "We sum up all elements from x and until y:"
      , ""
      , "x: " <> Text.pack (show x)
      , "y: " <> Text.pack (show y)
      , ""
      , "sum from x to y: " <> Text.pack (show s)
      ]
postHandler _ _ = throwError err404

getHandler :: ReaderT MyState Handler Double
getHandler = do
  db <- asks stateVar
  -- out <- liftIO $ readTVarIO db2
  -- liftIO $ print $ sum $ fmap (Text.length . snd) $ Map.assocs out
  liftIO $ putStrLn $ "Cleared database"
  liftIO $ atomically $ do
    -- writeTVar db2 Map.empty
    myMap <- readTVar db
    let
      s = sum $ fmap (listLengths . snd) $ Map.assocs myMap
      len = Map.size myMap
    pure $
      if len == 0
        then 0.0
        else fromIntegral s / fromIntegral len
