{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Control.Monad.IO.Class (liftIO)
import DB
import Data.Text
import Web.Scotty
import qualified Data.Text.Lazy as L

someFunc :: IO ()
someFunc = do
  putStrLn "starting server..."
  runDB
  scotty 3000 routes

routes :: ScottyM ()
routes = do
  get "/hello" hello
  get "/other" other

hello :: ActionM ()
hello = do
  person <- liftIO $ fmap toPerson <$> readDBPerson (DBFile "test.db") (Id 1)
  text $
    case person of
      Nothing -> "nothing"
      Just (Person n a) -> L.pack n

other :: ActionM ()
other = text "the other hello world"
