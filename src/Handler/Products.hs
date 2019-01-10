{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Products where

import System.Directory (getModificationTime)

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getProductsR :: Handler Html
getProductsR = getProductCategoryR "products"


{--
do
    entities <- runDB $ selectList [] []
    let categories = fmap entityVal entities
    modTimes <- liftIO $ forM categories (\(ProductCategory _ _ image _) ->
        maybe (return Nothing) (\x -> fmap Just $ getModificationTime ("static" </> "images" </> unpack x)) image)
    defaultLayout $ do
        setTitle "Product Catalogue"
        la <- formattedLanguages
        let langs = intercalate ", " la

        $(widgetFile "products")
--}

getProductCategoryR :: Text -> Handler Html
getProductCategoryR catText = do
    title <- if catText == "products"
                 then
                     return "Product Catalog"
                 else do
                     Entity _ (ProductCategory ident categoryTitle image priority) <- runDB $ getBy404 $ UniqueProductCategory catText
                     return categoryTitle
    defaultLayout $ do
        setTitle $ toHtml title
        la <- formattedLanguages
        let langs = intercalate ", " la

        let submenu = $(widgetFile "product-submenu")
        $(widgetFile "product-category")

getItemR :: Text -> Handler Html
getItemR item =
    defaultLayout $ do
        let submenu = $(widgetFile "product-submenu")
        if item == "sabbat" then $(widgetFile "item") else $(widgetFile "x12pro")
