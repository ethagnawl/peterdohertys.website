--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad               (forM)
import           Data.Char                   (toLower)
import           Data.List                   (sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (mappend)
import           Data.Ord                    (comparing)
import           Hakyll
import           Hakyll.Web.Sass             (sassCompiler)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "images/*" $ do
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

    match "index.html" $ do
        route idRoute
        compile $ do
            links <- sortByTitle =<< loadAll "links/*"
            let indexCtx =
                    listField "links" linkCtx (return links) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
linkCtx :: Context String
linkCtx =
    field "link" (return . itemBody) `mappend`
    defaultContext

sortByTitle :: MonadMetadata m => [Item a] -> m [Item a]
sortByTitle items = do
    itemsWithTitle <- forM items $ \item -> do
        maybeTitle <- getMetadataField (itemIdentifier item) "title"
        let title = fromMaybe "" maybeTitle
            uppercaseTitle = map toLower title
        return (uppercaseTitle, item)
    return (map snd $ sortBy (comparing fst) itemsWithTitle)
