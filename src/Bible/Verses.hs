{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bible.Verses(
      listVerses
    , html
    , reference
    , osisEnd
    , verseId
  ) where

import           Bible.Request
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Text           hiding (empty, foldl)
import           Lens.Family         hiding ((^.))
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

data Verse = Verse {
  _html      :: Text,
  _reference :: Text,
  _osisEnd   :: Text,
  _verseId   :: Text
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

data PlainTextVerse = PlainTextVerse {
  verse :: Text,
  body  :: Text
}

instance Show PlainTextVerse where
  show x = (show $ verse x) ++ "\n" ++ (show $ body x)
