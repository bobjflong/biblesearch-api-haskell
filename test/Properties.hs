{-# LANGUAGE OverloadedStrings #-}

module Properties where

import Bible.Verses
import Control.Lens hiding (elements)
import Test.QuickCheck
import Data.Text

instance Arbitrary Verse where
  arbitrary = do html <- genSafeText
                 reference <- genSafeText
                 osisEnd <- genSafeText
                 verseId <- genSafeText
                 return $ Verse html reference osisEnd verseId

instance Arbitrary Text where
  arbitrary = genSafeText

genSafeText :: Gen Text
genSafeText = fmap pack $ listOf $ elements ['a'..'z']

placingAndRetrievingVerses :: Verse -> Text -> Bool
placingAndRetrievingVerses v t = t == (plainText .~ t $ v) ^. plainText

settingWhatYouGet :: Verse -> Bool
settingWhatYouGet v = v == (plainText .~ (v ^. plainText) $ v)

settingTwice :: Verse -> Text -> Bool
settingTwice v t = (plainText .~ t $ v) == (plainText .~ t $ (plainText .~ t $ v))

main = do quickCheck placingAndRetrievingVerses
          >> quickCheck settingWhatYouGet
          >> quickCheck settingTwice
