{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Products where

import System.Directory (getModificationTime, listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath (splitExtension, splitPath)
import System.Random

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

itemsUrl = "dynamic" </> "items"

categoryTitles :: HashMap Text Text
categoryTitles = mapFromList [("headphones","Heaphones")
                             ,("speakers","Speakers")
                             ,("phoneCases","Phone Cases")
                             ,("drones","Drones")
                             ,("chargingCables","Charging Cables")
                             ,("wirelessChargers", "Wireless chargers")
                             ,("carPhoneMounts","Car Phone Mounts")]

getProductCategoryR :: Text -> Handler Html
getProductCategoryR catText = do
    if catText == "products"
    then do
        categories <- liftIO $ listDirectory itemsUrl
        itemData <- liftIO $ fmap join $ forM categories (productList . pack)
        let title = "Product Catalog" :: Text
        defaultLayout $ do
            setTitle $ "Product Catalog"
            let submenu = $(widgetFile "product-submenu")
            let numCols = "col-md-3" :: Text
            $(widgetFile "product-category")
    else do
        let categoryUrl = itemsUrl </> unpack catText
        exists <- liftIO $ doesDirectoryExist $ categoryUrl
        if exists
            then do
                itemData <- liftIO $ productList catText
                let title = fromMaybe catText (lookup catText categoryTitles)
                defaultLayout $ do
                    setTitle $ toHtml catText
                    let numCols = "col-md-6" :: Text
                    let submenu = $(widgetFile "product-submenu")
                    $(widgetFile "product-category")
            else
                notFound

productList :: Text -> IO [(Text, Text, Textarea, (Route App, [(Text,Text)]))]
productList catText = do
    let categoryUrl = itemsUrl </> unpack catText
    items <- listDirectory categoryUrl >>=
        filterM (\x -> do
            let itemUrl = categoryUrl </> x
            a <- doesFileExist $ itemUrl </> "name.txt"
            b <- doesFileExist $ itemUrl </> "description.txt"
            c <- doesFileExist $ itemUrl </> "cover.jpg"
            return $ a && b && c)
    forM items (\x -> do
        let itemUrl = categoryUrl </> x
        name <- readFileUtf8 $ itemUrl </> "name.txt"
        description <- fmap (fromString . unpack) (readFileUtf8 $ itemUrl </> "description.txt") :: IO Textarea
        etag <- randomIO :: IO Word64
        return (pack x, name, description, (DynamicImagesR catText (pack x) "cover.jpg", [("etag", pack $ show etag)])))
{--
do
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
--}
getItemR :: Text -> Handler Html
getItemR item = do
    p <- liftIO $ findItemDirectory item
    itemPath <- fromMaybe notFound (fmap return p)
    title <- liftIO $ readFileUtf8 $ itemPath </> "name.txt"
    pictures <- liftIO
        (fmap
            (sort . filter (\file ->
                let
                    (base,ext) = splitExtension file
                in
                    ext==".jpg" && base/="cover"))
            (listDirectory itemPath)) :: Handler [FilePath]

    let category = pack $ filter (/='/') $ penultimate $ splitPath itemPath :: Text
    pictureRoutes <- liftIO $ forM pictures (\p -> do
        etag <- randomIO :: IO Word64
        return (DynamicImagesR category item (pack p), [("etag", pack $ show etag)]))

    defaultLayout $ do
        let submenu = $(widgetFile "product-submenu")
        $(widgetFile "item")

findItemDirectory :: Text -> IO (Maybe FilePath)
findItemDirectory item = do
    categories <- listDirectory itemsUrl
    matching <- filterM doesDirectoryExist [itemsUrl </> c </> unpack item | c <- categories]
    return $ headMay matching

penultimate :: [a] -> a
penultimate [x,_] = x
penultimate (_:x:y:xs) = penultimate (x:y:xs)
