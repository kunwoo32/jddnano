{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.DynamicImages where

import Import

itemsUrl = "dynamic" </> "items"

getDynamicImagesR :: Text -> Text -> Text -> Handler Html
getDynamicImagesR c p f =
    if takeExtension (unpack f) == ".jpg"
        then
            sendFile typeJpeg $ itemsUrl </> unpack c </> unpack p </> unpack f
        else
            notFound

takeExtension :: FilePath -> String
takeExtension [] = ""
takeExtension ('.' : xs) = '.' : xs
takeExtension (_ : xs) = takeExtension xs
