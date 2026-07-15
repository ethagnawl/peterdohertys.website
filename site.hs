{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM)
import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Ord (Down (..), comparing)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Hakyll.Web.Tags (getTags)

main :: IO ()
main = hakyll $ do
  match ("images/**" .||. "favicon.ico") $ do
    route idRoute
    compile copyFileCompiler
  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler
  -- inspired by: https://github.com/meoblast001/meosite/blob/master/Site.hs
  match "css/*.sass" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)
  match "links/*" $ compile pandocCompiler
  match (fromList ["about-me.markdown", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
  match "case-studies/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/case-study.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
  match "case-studies.html" $ do
    route idRoute
    compile $ do
      caseStudies <- loadAll "case-studies/*"
      let indexCtx =
            listField "caseStudies" defaultContext (return caseStudies)
              <> constField "title" "Case Studies"
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match "blog-posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/blog-post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
  match "blog-posts.html" $ do
    route idRoute
    compile $ do
      blogPosts <- sortByPublishedOn =<< loadAll "blog-posts/*"
      let indexCtx =
            listField "blogPosts" defaultContext (return blogPosts)
              <> constField "title" "Blog Posts"
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match "index.html" $ do
    route idRoute
    compile $ do
      links <- sortByTitle =<< loadAll "links/*"
      let indexCtx =
            listField "links" linkCtx (return links)
              <> constField "title" "Home"
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
linkCtx :: Context String
linkCtx =
  field "link" (return . itemBody) <> defaultContext

sortByTitle :: MonadMetadata m => [Item a] -> m [Item a]
sortByTitle items = do
  itemsWithTitle <- forM items $ \item -> do
    maybeTitle <- getMetadataField (itemIdentifier item) "title"
    let title = fromMaybe "" maybeTitle
        uppercaseTitle = map toLower title
    return (uppercaseTitle, item)
  return (map snd $ sortBy (comparing fst) itemsWithTitle)

sortByPublishedOn :: MonadMetadata m => [Item a] -> m [Item a]
sortByPublishedOn items = do
  itemsWithDate <- forM items $ \item -> do
    maybeDate <- getMetadataField (itemIdentifier item) "published_on"
    let parsed = maybeDate >>= parseTimeM True defaultTimeLocale "%-m/%-d/%Y"
        day = fromMaybe (toEnum 0) parsed :: Day
    return (day, item)
  return $ map snd $ sortBy (comparing (Down . fst)) itemsWithDate

postCtx :: Context String
postCtx = hasTagField "hasDuckDbTag" "duckdb" <> defaultContext

hasTagField :: String -> String -> Context a
hasTagField fieldName tag = field fieldName $ \item -> do
  tags <- getTags (itemIdentifier item)
  if tag `elem` tags
    then return "true"
    else fail ("Item does not have tag \"" ++ tag ++ "\"")
