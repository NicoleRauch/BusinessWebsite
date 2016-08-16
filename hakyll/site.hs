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
import           StringManipulation (getExcerpt)

--------------------------------------------------------------------------------
siteConfig :: Configuration
siteConfig =  defaultConfiguration {
              destinationDirectory = "../HTML"
            , storeDirectory       = "../_hakyll_cache"
            , tmpDirectory         = "../_hakyll_cache/tmp"
           }

main :: IO ()
main = hakyllWith siteConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "robots.txt" $ do
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

    match "data/termine/vergangene.json" $ do
        compile $ do
            lbs <- fmap itemBody getResourceLBS
            case A.eitherDecode lbs of
                 Left err -> fail err
                 Right ys -> makeItem (ys::Years) 

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= relativizeUrls

    match "posts/*" $ version "raw" $ do
        route   $ setExtension "html"   -- we need to "cheat" here in order to get the correct URLs from the $url$ tag
        compile getResourceBody

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
            let 
                postList = return posts
                blogCtx =
                    listField "germanPosts" postWithExcerptCtx (german postList) `mappend`
                    listField "englishPosts" postWithExcerptCtx (english postList) `mappend`
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
                yearItems = do
                          Years years <- loadBody "data/termine/vergangene.json"
                          mapM makeItem years
                indexCtx =
                    constField "title" "Home"                `mappend`
                    listField "kommende" terminCtx terminItems                 `mappend`
                    listField "vergangene" yearCtx yearItems                   `mappend`
                    generalContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

-- Termine management:

data Termin = Termin {
     terminName :: String
     , terminTitle :: String
     , terminUrl :: Maybe String
     , terminVideoUrl :: Maybe String
     , terminKind :: String
} deriving (Generic)

terminCtx :: Context Termin
terminCtx = mconcat [field "name" (return . terminName . itemBody),
                     field "title" (return . terminTitle . itemBody),
                     field "url" (maybe (fail "") return . terminUrl . itemBody),
                     field "videoUrl" (maybe (fail "") return . terminVideoUrl . itemBody),
                     field "kind" (return . terminKind . itemBody)
                     ]

instance Binary Termin

instance A.FromJSON Termin where
    parseJSON = A.withObject "Can't parse Termin" $ \obj -> do
              name <- obj .: "name"
              title <- obj .: "title"
              url <- obj .:? "url"
              videoUrl <- obj .:? "videoUrl"
              kind <- obj .: "kind"
              return $ Termin name title url videoUrl kind

newtype Termine = Termine {
        unTermine :: [Termin]
} deriving (Generic, A.FromJSON)

instance Binary Termine

instance Writable Termine
   where write _ _ = return ()

--------------------------------------------------------------------------------

-- Years containing Termine:

data Year = Year {
     yearYear :: String
     , yearEntries :: Termine
} deriving (Generic)

yearCtx :: Context Year
yearCtx = mconcat
    [ field         "year"    (return . yearYear . itemBody)
    , listFieldWith "entries" terminCtx $ \yearItem -> do
        let Termine entries = yearEntries (itemBody yearItem)
        mapM makeItem entries
    ]

instance Binary Year

instance A.FromJSON Year where
         parseJSON = A.withObject "Can't parse Year" $ \obj -> do
                   year <- obj .: "year"
                   entries <- obj .: "entries"
                   return $ Year year entries

newtype Years = Years {
        unYears :: [Year]
} deriving (Generic, A.FromJSON)

instance Binary Years

instance Writable Years
         where write _ _ = return ()

--------------------------------------------------------------------------------

-- Multi-Language blog post management:

german :: Compiler [Item String] -> Compiler [Item String]
german c = c >>= filterByLang "de"

english :: Compiler [Item String] -> Compiler [Item String]
english c = c >>= filterByLang "en"

filterByLang :: String -> [Item String] -> Compiler [Item String]
filterByLang lang posts = filterM (hasLang lang) posts

hasLang :: String -> Item String -> Compiler Bool
hasLang lang item = fmap ((==) lang) $ langOfItem item

langOfItem :: Item String -> Compiler String
langOfItem item = langOfPost $ itemIdentifier item

langOfPost :: Identifier -> Compiler String
langOfPost id = getMetadataField' id "lang"

--------------------------------------------------------------------------------
-- Blog Post Excerpts (i.e. the first paragraph):

getResourceBodyExcerpt :: Item String -> Compiler String
getResourceBodyExcerpt item = return $ getExcerpt $ itemBody item

--------------------------------------------------------------------------------

-- Contexts:

postCtx :: Context String
postCtx =
    dateField "date" "%-d.%m.%Y" `mappend`
    generalContext

postWithExcerptCtx :: Context String
postWithExcerptCtx =
    field "excerpt" getResourceBodyExcerpt `mappend`
    postCtx

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
