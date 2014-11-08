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

main = do quickCheck placingAndRetrievingVerses
