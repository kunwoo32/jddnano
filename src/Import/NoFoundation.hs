{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Import.NoFoundation
    ( module Import,
      formattedLanguages
    ) where

import ClassyPrelude.Yesod   as Import
import Model                 as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Data.List (nubBy)

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
