--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.Aeson as A
import           Data.Aeson ((.:), (.:?))
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath.Posix (dropExtension, splitDirectories)
import           GHC.Generics (Generic)
import           Data.Binary (Binary)
import           Control.Monad (filterM)

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

    match "data/termine/kommende.json" $ do
        compile $ do
            lbs <- fmap itemBody getResourceLBS
            case A.eitherDecode lbs of
                 Left err -> fail err
                 Right ts -> makeItem (ts::Termine) 

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


    match (fromList ["index.html", "impressum.html", "referenzen.html", "schwerpunkte.html", "themen.html"]) $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home"                `mappend`
                    generalContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "termine.html" $ do
        route idRoute
        compile $ do
            
            let terminItems = do
                           Termine termine  <- loadBody "data/termine/kommende.json"
                           mapM makeItem termine
                indexCtx =
                    constField "title" "Home"                `mappend`
                    listField "kommende" terminCtx terminItems                 `mappend`
                    generalContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

data Termin = Termin {
     terminName :: String
     , terminTitle :: String
     , terminUrl :: Maybe String
     , terminKind :: String
} deriving (Generic)

terminCtx :: Context Termin
terminCtx = mconcat [field "name" (return . terminName . itemBody),
                     field "title" (return . terminTitle . itemBody),
                     field "url" (maybe (fail "") return . terminUrl . itemBody),
                     field "kind" (return . terminKind . itemBody)
                     ]

instance Binary Termin

instance A.FromJSON Termin where
    parseJSON = A.withObject "Can't parse Termin" $ \obj -> do
              name <- obj .: "name"
              title <- obj .: "title"
              url <- obj .:? "url"
              kind <- obj .: "kind"
              return $ Termin name title url kind

newtype Termine = Termine {
        unTermine :: [Termin]
} deriving (Generic, A.FromJSON)

instance Binary Termine

instance Writable Termine
   where write _ _ = return ()

-- what I need:

german :: Compiler [Item String] -> Compiler [Item String]
-- german c = fmap (langFilter "de") c
-- german c = fmap (langFilter2 "de") c
german c = c >>= langFilter2a "de"

langFilter2a :: String -> [Item String] -> Compiler [Item String]
langFilter2a lang posts = filterM isGerman2 posts

langFilter2 :: String -> [Item a] -> [Item a]
langFilter2 lang [] = []
langFilter2 lang (x:xs) = (langFilter3 lang x) ++ (langFilter2 lang xs)

langFilter3 :: String -> Item a -> [Item a]
langFilter3 lang item = [item]

--  {% if post.lang == "en" %}
english :: Compiler [Item a] -> Compiler [Item a]
english c = fmap (langFilter "en") c

langFilter :: String -> [Item a] -> [Item a]
langFilter lang x = x

langOfItem :: Item String -> Compiler String
langOfItem item = langOfPost $ itemIdentifier item

isGerman2 :: Item String -> Compiler Bool
isGerman2 item = fmap ((==) "de") $ langOfItem item



langOfPost :: MonadMetadata m => Identifier -> m String
langOfPost id = getMetadataField' id "lang"

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
