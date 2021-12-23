{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp,
  )
where

import Control.Monad
import Data.Text (Text)
import Data.Time.Clock
import Database.Selda
import Database.Selda.SQLite
import System.Directory
import System.Environment

dbName :: String
dbName = "test.sqlite"

data User = User
  { userId :: ID User,
    age :: Int,
    userName :: Text
  }
  deriving (Eq, Show, Generic)

instance SqlRow User

usersTable :: Table User
usersTable = table "users" [#userId :- autoPrimary]

data Book = Book
  { bookId :: ID Book,
    bookAuthorId :: ID User,
    bookTitle :: Text
  }
  deriving (Eq, Show, Generic)

instance SqlRow Book

booksTable :: Table Book
booksTable = table "books" [#bookId :- autoPrimary, #bookAuthorId :- foreignKey usersTable #userId]

initDB :: IO ()
initDB = withSQLite dbName $ do
  createTable usersTable
  createTable booksTable

deleteDBIfExists :: IO ()
deleteDBIfExists =
  do
    fileExists <- doesFileExist dbName
    when fileExists (removeFile dbName)

startApp :: IO ()
startApp =
  do
    putStrLn "Initializing DB..."

    deleteDBIfExists

    initDB

    testQueries

testQueries :: IO ()
testQueries = withSQLite dbName $ do
  -- liftIO $ putStrLn "\nInsert users..."
  -- insert_
  --   usersTable
  --   [ User def 42 "Velvet",
  --     User def 44 "Kobayashi",
  --     User def 55 "Miyu"
  --   ]

  -- users <- query $ select usersTable

  -- liftIO $ putStrLn "\nInserted users..."
  -- liftIO $ forM_ users print

  -- forM_
  --   users
  --   ( \user ->
  --       insert_
  --         booksTable
  --         [Book def (userId user) "Velvet"]
  --   )

  -- liftIO $ putStrLn "\nInsert books..."
  -- books <- query $ select booksTable

  -- liftIO $ putStrLn "\nInserted books..."
  -- liftIO $ forM_ books print

  liftIO $ putStrLn "\nCurrent time..."
  liftIO $ getCurrentTime >>= print

  liftIO $ putStrLn "Inserting 30000 users..."

  insert_ usersTable (map (\_ -> User def 42 "random") [1 .. 30000])

  liftIO $ putStrLn "Finished time..."
  liftIO $ getCurrentTime >>= print

  liftIO $ putStrLn "\nCurrent time..."
  liftIO $ getCurrentTime >>= print

  liftIO $ putStrLn "Incrementing age of users..."
  update usersTable (\user -> user ! #userName .== "random") (\user -> user `with` [#age += 1])

  liftIO $ putStrLn "Finished time..."
  liftIO $ getCurrentTime >>= print

  return ()
