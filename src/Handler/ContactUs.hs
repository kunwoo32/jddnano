{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ContactUs where

import Prelude (cycle)
import System.Directory (getModificationTime, listDirectory, doesDirectoryExist, doesFileExist)

import Import


getContactUsR :: Handler Html
getContactUsR = do
    staff <- liftIO $ getStaff
    defaultLayout $ do
        setTitle "Contact Us"
        $(widgetFile "contact-us")

staffUrl = "dynamic" </> "staff"

getStaff :: IO [((Route App, [(Text,Text)]), Text, Text, Text)]
getStaff = do
    s <- listDirectory staffUrl >>=
        filterM (\x -> do
            let personUrl = staffUrl </> x
            a <- doesFileExist $ personUrl </> "name.txt"
            b <- doesFileExist $ personUrl </> "role.txt"
            c <- doesFileExist $ personUrl </> "profile.jpg"
            return $ a && b && c)
    let staff = sort s
    info <- forM staff (\x -> do
        let personUrl = staffUrl </> x
        name <- readFileUtf8 $ personUrl </> "name.txt"
        role <- readFileUtf8 $ personUrl </> "role.txt"
        etag <- makeImageEtag
        return ((DynamicStaffImagesR (pack x) "profile.jpg", [("etag", etag)]), name, role))
    return $ zipWith (\(a,b,c) d -> (a,b,c,d)) info (cycle ["reversed", ""])
