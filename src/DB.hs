{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
DBPerson
  name String
  age Int Maybe
  deriving Show
|]

data Person = Person
  { name :: String
  , age :: Maybe Int
  } deriving (Show)

newtype DBFile = DBFile
  { unDBFile :: String
  } deriving (Show, Eq)

newtype Id = Id
  { unId :: Int
  } deriving (Show, Eq)

toText :: DBFile -> Text
toText = pack . unDBFile

runDB :: IO ()
runDB =
  runSqlite "test.db" $ do
    runMigration migrateAll
    johnId <- insert $ DBPerson "John" $ Just 40
    john <- get $ DBPersonKey 4
    liftIO $ print (john :: Maybe DBPerson)

readDBPerson :: DBFile -> Id -> IO (Maybe DBPerson)
readDBPerson d i =
  runSqlite (toText d) $ do
    runMigration migrateAll
    john <- get $ DBPersonKey _
    liftIO $ pure (john :: Maybe DBPerson)

toPerson :: DBPerson -> Person
toPerson (DBPerson n a) = Person n a
