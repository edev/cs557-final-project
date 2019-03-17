{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BasicPage where

import Import

getBasicPageR :: PagePath -> Handler Html
getBasicPageR (PagePath pagePath) = do
      maybePage <- runDB $ getBy (UniquePath (Just pagePath))
      case maybePage of
        Just (Entity _ (BasicPage title _ _ _ content)) 
          -> defaultLayout $ do
                setTitle (toHtml title)
                $(widgetFile "basicpage")
        Nothing -> notFound -- No page with this path exists in the DB.

