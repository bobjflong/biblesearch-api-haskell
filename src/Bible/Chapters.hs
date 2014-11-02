{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bible.Chapters(
      listChapters
    , chapter
    , osisEnd
    , chapterId
  ) where

import           Bible.Request
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Text           hiding (empty, filter)
import           Lens.Family

data Chapter = Chapter {
  _chapterData :: String,
  _osisEnd     :: String,
  _chapterId   :: String
} deriving (Show, Eq)

data ChapterNumber = ChapterNumber Int | UnknownChapter deriving (Show)

$(makeLenses ''Chapter)

chapter :: Lens' Chapter ChapterNumber
chapter = lens getChapter setChapter
  where getChapter c = case (reads (_chapterData c) :: [(Int, String)]) of
                         [(a,"")] -> ChapterNumber a
                         _ -> UnknownChapter
        setChapter c (ChapterNumber x) = c { _chapterData = (show x) }
        setChapter c UnknownChapter = c { _chapterData = "" }

instance FromJSON Chapter where
  parseJSON (Object v) = do
    Chapter <$> v .: "chapter"
            <*> v .: "osis_end"
            <*> v .: "id"
  parseJSON _ = empty

data ChapterList = ChapterList {
  chapters :: [Chapter]
} deriving (Show)

instance FromJSON ChapterList where
  parseJSON (Object v) = do
    ChapterList <$> ((v .: "response") >>= (.: "chapters"))

listChapters :: Text -> ApiKey -> IO (Maybe [Chapter])
listChapters bookId key = do
  raw <- getResourceAtPath path key
  return $ fmap (pruneBadResults.chapters) $ decode raw
  where path = "/books/" `append` bookId `append` "/chapters.js"

-- API seems to return some garbled data
pruneBadResults :: [Chapter] -> [Chapter]
pruneBadResults c = filter notGarbled c
  where notGarbled chapter = (_chapterData chapter) /= "int"

