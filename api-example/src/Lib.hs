{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import Data.List
import Data.Text (Text)
import Database.SQLite.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-----------------------------------------
-------------- MODELS -------------------
-----------------------------------------

data User = User
  { userId :: Int,
    userName :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance FromRow User where
  fromRow = User <$> field <*> field

data Book = Book
  { bookId :: Int,
    authorId :: Int,
    bookTitle :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Book)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field

-----------------------------------------
-------------- API ----------------------
-----------------------------------------

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "users" :> Capture "id" Int :> Get '[JSON] User
    :<|> "users" :> Capture "id" Int :> "books" :> Get '[JSON] [Book]

type BookAPI =
  "books" :> Get '[JSON] [Book]
    :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Int64

type API = UserAPI :<|> BookAPI

initDB :: Connection -> IO ()
initDB conn =
  do
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, author INTEGER, title TEXT)"

startApp :: IO ()
startApp =
  do
    print "Initializing DB..."
    conn <- open "test.db"
    initDB conn
    print "Running Server..."
    run 8080 (app conn)

app :: Connection -> Application
app conn =
  serve
    api
    $ usersAPI conn
      :<|> booksAPI conn

api :: Proxy API
api = Proxy

-----------------------------------------
-------------- USER API -----------------
-----------------------------------------

usersAPI :: Connection -> Server UserAPI
usersAPI conn =
  getAllUsersAPI conn
    :<|> createUserAPI conn
    :<|> findUserAPI conn
    :<|> booksOfUserAPI conn

getAllUsersAPI :: Connection -> Handler [User]
getAllUsersAPI conn = liftIO $ allUsers conn

createUserAPI :: Connection -> User -> Handler Int64
createUserAPI conn user = liftIO $ createUser conn user

findUserAPI :: Connection -> Int -> Handler User
findUserAPI conn userId =
  do
    results <- liftIO $ findUser conn userId
    if null results
      then do
        let err = err404 {errBody = "user not found"}
        liftIO $ print err
        throwError err
      else return (head results)

booksOfUserAPI :: Connection -> Int -> Handler [Book]
booksOfUserAPI conn userId = liftIO (booksOfUser conn userId)

-----------------------------------------
-------------- BOOKS API ----------------
-----------------------------------------

booksAPI :: Connection -> Server BookAPI
booksAPI conn = getAllBooksAPI conn :<|> createBookAPI conn

getAllBooksAPI :: Connection -> Handler [Book]
getAllBooksAPI conn = liftIO (allBooks conn)

createBookAPI :: Connection -> Book -> Handler Int64
createBookAPI conn book = liftIO (createBook conn book)

-----------------------------------------
-------------- USER LOGIC ---------------
-----------------------------------------

allUsers :: Connection -> IO [User]
allUsers conn = query_ conn "SELECT * from users"

createUser :: Connection -> User -> IO Int64
createUser conn user =
  do
    execute conn "INSERT INTO users (name) VALUES (?)" (Only (userName user))
    lastInsertRowId conn

findUser :: Connection -> Int -> IO [User]
findUser conn userId = query conn "SELECT * from users WHERE id = ?" (Only userId)

booksOfUser :: Connection -> Int -> IO [Book]
booksOfUser conn userId = query conn "SELECT * from books where author = ?" (Only userId)

-----------------------------------------
-------------- BOOKS LOGIC --------------
-----------------------------------------

allBooks :: Connection -> IO [Book]
allBooks conn = query_ conn "SELECT * from books"

createBook :: Connection -> Book -> IO Int64
createBook conn book =
  do
    execute conn "INSERT INTO books (author, title) VALUES (?, ?)" (authorId book, bookTitle book)
    lastInsertRowId conn
