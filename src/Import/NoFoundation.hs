{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Import.NoFoundation
    ( module Import,
      formattedLanguages
    , makeImageEtag
    ) where

import ClassyPrelude.Yesod   as Import
import Model                 as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Data.List (nubBy)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder (toLazyByteString, word64BE)
import qualified Data.ByteString.Lazy as BL
import System.Random

-- Adds zh-Hans and zh-Hant when appropriate.
-- Also removes duplicates.
formattedLanguages :: MonadHandler m => m [Text]
formattedLanguages = fmap (nubBy (~~) . format) languages
    where
        format [] = []
        format (x:xs)
            | x ~~ "zh-CN" = x : "zh-Hans" : "zh" : format xs
            | x ~~ "zh-SG" = x : "zh-Hans" : "zh" : format xs
            | x ~~ "zh-MY" = x : "zh-Hans" : "zh" : format xs
            | x ~~ "zh-TW" = x : "zh-Hant" : "zh" : format xs
            | x ~~ "zh-HK" = x : "zh-Hant" : "zh" : format xs
            | x ~~ "zh-MO" = x : "zh-Hant" : "zh" : format xs
            | otherwise = x : format xs

-- case insensitive comparison
(~~) :: (Eq t, Textual t) => t -> t -> Bool
(~~) a b = toLower a == toLower b

makeImageEtag :: IO Text
makeImageEtag = do
    word <- randomIO :: IO Word64
    return $ pack $ filter (/='=') $ C.unpack $ Data.ByteString.Base64.URL.encode $ B.dropWhile (==0) $ BL.toStrict $ toLazyByteString $ word64BE word
