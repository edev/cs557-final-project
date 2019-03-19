{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BasicPage where

import Import

getBasicPageR :: PagePath -> Handler Html
getBasicPageR (PagePath pagePath) = do
      -- Try to retrieve the page from the database by looking up
      -- the path we received from the BasicPageR route.
      maybePage <- runDB $ getBy (UniquePath (Just pagePath))
      case maybePage of
        -- We received a valid page! Great!
        -- We'll unpack just what we need, discarding the rest,
        -- and render the layout by following the same basic steps
        -- as the default HomePageR handler.
        Just (Entity _ (BasicPage title _ _ _ content)) 
          -> defaultLayout $ do
                setTitle (toHtml title)
                $(widgetFile "basicpage")
        Nothing -> notFound -- No page with this path exists in the DB.

