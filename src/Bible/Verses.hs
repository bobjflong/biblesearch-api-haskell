{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bible.Verses(
      listVerses
    , html
    , plainText
    , reference
    , osisEnd
    , verseId
  ) where

import           Bible.Request
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Char
import           Data.Text           hiding (empty, foldl, map)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import           Lens.Family         hiding ((^.))
import           Text.Taggy

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

plainText :: Lens' Verse Text
plainText = lens getPlainText setPlainText
  where getPlainText v = verseHTMLToPlainText $ v ^. html
        setPlainText v t = v { _html = plainTextToVerseHTML t }

verseHTMLToPlainText :: Text -> Text
verseHTMLToPlainText html = tagsToTextExcludingNumbers $ taggyWith True (TL.fromStrict html)

plainTextToVerseHTML :: Text -> Text
plainTextToVerseHTML = template
  where template x = T.concat ["<p>", x, "</p>"]

tagsToTextExcludingNumbers :: [Tag] -> Text
tagsToTextExcludingNumbers t = foldl append "" (map textFromTags t)
  where textFromTags (TagText x) = case (T.all isNumber x) of
                                     True -> ""
                                     False -> x
        textFromTags _ = ""

