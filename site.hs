--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
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

    match "index.html" $ do
        route idRoute
        compile $ do
            links <- loadAll "links/*"
            let indexCtx =
                    listField "links" linkCtx (return links) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "links/*" $ compile pandocCompiler

--------------------------------------------------------------------------------
linkCtx :: Context String
linkCtx =
    field "link" $ \item -> return (itemBody item)
    defaultContext

