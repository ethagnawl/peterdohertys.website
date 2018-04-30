{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad               (forM)
import           Data.Char                   (toLower)
import           Data.List                   (sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.Ord                    (comparing)
import           Hakyll
import           Hakyll.Web.Sass             (sassCompiler)

main :: IO ()
main = hakyll $ do

    match ("images/*" .||. "favicon.ico") $ do
        route   idRoute
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
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "case-studies/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/case-study.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "case-studies.html" $ do
        route idRoute
        compile $ do
            caseStudies <- loadAll "case-studies/*"
            let indexCtx = listField "caseStudies" defaultContext (return caseStudies) <>
                           constField "title" "Case Studies" <>
                           defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "demos/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/demo.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "demos.html" $ do
        route idRoute
        compile $ do
            demos <- loadAll "demos/*"
            let indexCtx = listField "demos" defaultContext (return demos) <>
                           constField "title" "Demos" <>
                           defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            links <- sortByTitle =<< loadAll "links/*"
            let indexCtx =
                    listField "links" linkCtx (return links) <>
                    constField "title" "Home"                <>
                    defaultContext

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
