{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.DynamicImages where

import Import

itemsUrl = "dynamic" </> "items"

getDynamicItemImagesR :: Text -> Text -> Text -> Handler Html
getDynamicItemImagesR c p f =
    if takeExtension (unpack f) == ".jpg"
        then
            sendFile typeJpeg $ itemsUrl </> unpack c </> unpack p </> unpack f
        else
            notFound

staffUrl = "dynamic" </> "staff"

getDynamicStaffImagesR :: Text -> Text -> Handler Html
getDynamicStaffImagesR c f =
    if takeExtension (unpack f) == ".jpg"
        then
            sendFile typeJpeg $ staffUrl </> unpack c </> unpack f
        else
            notFound

takeExtension :: FilePath -> String
takeExtension [] = ""
takeExtension ('.' : xs) = '.' : xs
takeExtension (_ : xs) = takeExtension xs
