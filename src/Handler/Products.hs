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
getProductsR = do
    entities <- runDB $ selectList [] []
    let categories = fmap entityVal entities
    modTimes <- liftIO $ forM categories (\(ProductCategory _ _ image _) ->
        maybe (return Nothing) (\x -> fmap Just $ getModificationTime ("static" </> "images" </> unpack x)) image)
    defaultLayout $ do
        setTitle "Product Catalogue"
        la <- formattedLanguages
        let langs = intercalate ", " la

        $(widgetFile "products")

getProductCategoryR :: Text -> Handler Html
getProductCategoryR _ = getProductsR
{-
do
    let matching = [c | c <-categories, productCategoryIdent c == cat]
    maybe
        notFound
        (\category -> defaultLayout $ do
            setTitle $ toHtml $ productCategoryLabel category
            $(widgetFile "productCategory"))
        (headMay matching)

data ProductCategory = ProductCategory
    { productCategoryIdent :: Text
    , productCategoryLabel :: Text
    , productCategoryImage :: Maybe Text
    }

data Product = Product
    { productIdent :: Text
    , productImage :: Maybe Text
    , productDescription :: Maybe Text
    , productCategory :: ProductCategory
    }

categories = [headphones, speakers, cases, drones, cables]

headphones = ProductCategory
    { productCategoryIdent = "headphones"
    , productCategoryLabel = "Headphones"
    , productCategoryImage = Just "categoryHeadphones.jpg"
    }
speakers = ProductCategory
    { productCategoryIdent = "speakers"
    , productCategoryLabel = "Speakers"
    , productCategoryImage = Just "categorySpeakers.jpg"
    }
cases = ProductCategory
    { productCategoryIdent = "phoneCases"
    , productCategoryLabel = "Phone Cases"
    , productCategoryImage = Just "categoryCases.jpg"
    }
drones = ProductCategory
    { productCategoryIdent = "drones"
    , productCategoryLabel = "Drones"
    , productCategoryImage = Just "categoryDrones.jpg"
    }
cables = ProductCategory
    { productCategoryIdent = "chargingCables"
    , productCategoryLabel = "Charging Cables"
    , productCategoryImage = Just "categoryCables.jpg"
    }
-}
