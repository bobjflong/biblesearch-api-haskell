{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Bible.Versions(
      listVersions
    , versionId
    , name
    , lang
    , langCode
    , contactUrl
  ) where

import           Bible.Request
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString     as B
import           Data.Text           hiding (empty)
import           Debug.Trace
import           Lens.Family

data Version = Version {
  _versionId  :: String,
  _name       :: String,
  _lang       :: String,
  _langCode   :: String,
  _contactUrl :: String
} deriving (Show, Eq)

$(makeLenses ''Version)

instance FromJSON Version where
  parseJSON (Object v) = do
    Version <$> v .: "id"
            <*> v .: "name"
            <*> v .: "lang"
            <*> v .: "lang_code"
            <*> v .: "contact_url"
  parseJSON _ = empty

data VersionList = VersionList {
  versions :: [Version]
} deriving (Show)

instance FromJSON VersionList where
  parseJSON (Object v) = do
    VersionList <$> ((v .: "response") >>= (.: "versions"))

listVersions :: ApiKey -> IO (Maybe [Version])
listVersions key = do
  raw <- getResourceAtPath path key
  return $ fmap versions $ decode raw
  where path = "/versions.js"

