{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bible.Verses(
      listVerses
    , text
    , reference
    , osisEnd
    , verseId
  ) where

import           Bible.Request
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Text           hiding (empty)
import           Lens.Family

data Verse = Verse {
  _text      :: String,
  _reference :: String,
  _osisEnd   :: String,
  _verseId   :: String
} deriving (Show, Eq)

$(makeLenses ''Verse)

instance FromJSON Verse where
  parseJSON (Object v) = do
    Verse <$> v .: "text"
            <*> v .: "reference"
            <*> v .: "osis_end"
            <*> v .: "id"
  parseJSON _ = empty

data VerseList = VerseList {
  verses :: [Verse]
} deriving (Show)

instance FromJSON VerseList where
  parseJSON (Object v) = do
    VerseList <$> ((v .: "response") >>= (.: "verses"))

listVerses :: Text -> ApiKey -> IO (Maybe [Verse])
listVerses chapterId key = do
  raw <- getResourceAtPath path key
  return $ fmap verses $ decode raw
  where path = "/chapters/" `append` chapterId `append` "/verses.js"

