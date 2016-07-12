--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad               (forM)
import           Data.Char                   (toLower)
import           Data.List                   (sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (mappend)
import           Data.Ord                    (comparing)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

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
