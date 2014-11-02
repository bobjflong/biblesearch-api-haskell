{-# LANGUAGE OverloadedStrings #-}

module Bible.Request(
    ApiKey,
    getResourceAtPath
  ) where

import           Control.Lens
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LBS
import           Data.Text
import qualified Data.Text.Encoding   as DE
import qualified Data.Text.Lazy       as TL

import           Data.Text.Encoding
import           Network.Wreq

baseUrl :: Text
baseUrl = "https://bibles.org/v2"

type ApiKey = Text

requestOptions :: ApiKey -> Options
requestOptions key = defaults & setAuth & setContentType & setAccept
  where setAuth = auth .~ (basicAuth (encodeUtf8 key) "X")
        setContentType = header "Content-Type" .~ ["application/json"]
        setAccept = header "Accept" .~ ["application/json"]

getResourceAtPath :: Text -> ApiKey -> IO LBS.ByteString
getResourceAtPath path key = do
  r <- getWith (requestOptions key) (unpack url)
  return $ r ^. responseBody
  where url = baseUrl `append` path
