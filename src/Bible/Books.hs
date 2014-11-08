{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bible.Books(
      listBooks
    , name
    , bookId
    , osisEnd
    , testament
    , Testament(..)
  ) where

import           Bible.Request
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Text           hiding (empty)
import           Debug.Trace
import           Lens.Family

data Testament = OldTestament | NewTestament | Unknown deriving (Show, Eq)

data Book = Book {
  _name          :: Text,
  _bookId        :: Text,
  _osisEnd       :: Text,
  _testamentData :: Text
} deriving (Show, Eq)

$(makeLenses ''Book)

testament :: Lens' Book Testament
testament = lens getTestament setTestament
  where getTestament b = case (_testamentData b) of
          "OT" -> OldTestament
          "NT" -> NewTestament
          _ -> Unknown
        setTestament b t = case t of
          OldTestament -> b { _testamentData = "OT" }
          NewTestament -> b { _testamentData = "NT" }
          _ -> b { _testamentData = "Unknown" }

instance FromJSON Book where
  parseJSON (Object v) = do
    Book <$> v .: "name"
            <*> v .: "id"
            <*> v .: "osis_end"
            <*> v .: "testament"
  parseJSON _ = empty

data BookList = BookList {
  books :: [Book]
} deriving (Show)

instance FromJSON BookList where
  parseJSON (Object v) = do
    BookList <$> ((v .: "response") >>= (.: "books"))

listBooks :: Text -> ApiKey -> IO (Maybe [Book])
listBooks version key = do
  raw <- getResourceAtPath path key
  return $ fmap books $ decode raw
  where path = "/versions/" `append` version `append` "/books.js"

