--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath.Posix (dropExtension, splitDirectories)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" generalContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    generalContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let 
                postList = return posts
                blogCtx =
                    listField "germanPosts" postCtx (german postList) `mappend`
                    listField "englishPosts" postCtx (english postList) `mappend`
                    generalContext

            getResourceBody
                >>= applyAsTemplate blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home"                `mappend`
                    generalContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

german :: Compiler [Item a] -> Compiler [Item a]
german c = fmap (langFilter "de") c

--  {% if post.lang == "en" %}
english :: Compiler [Item a] -> Compiler [Item a]
english c = fmap (langFilter "en") c

langFilter :: String -> [Item a] -> [Item a]
langFilter lang x = x

postCtx :: Context String
postCtx =
    dateField "date" "%-d.%m.%Y" 
                          `mappend`
    generalContext

activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] _ -> do
         path <- toFilePath <$> getUnderlying
         return $ if dropExtension (head (splitDirectories path)) == p
                     then "class=\"active\""
                     else ""

generalContext :: Context String
generalContext =
               activeClassField `mappend`
               defaultContext
