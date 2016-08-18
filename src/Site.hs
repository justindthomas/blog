{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving, AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative ( (<|>) )
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Logger
import           Data.Maybe
import           Data.Time
import           Data.ByteString (ByteString)
import           Data.Binary
import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.Sass
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Compiled.LowLevel as C
import           Text.Pandoc
import           Text.Pandoc.Error (handleError)
import qualified Database.Groundhog as G
import           Database.Groundhog.Core
import           Database.Groundhog.TH
import           Database.Groundhog.Postgresql
import           Snap.Snaplet.PostgresqlSimple (getConnectionString)
------------------------------------------------------------------------------
import           Application

data Article = Article {
  reference  :: T.Text,
  title      :: T.Text,
  summary    :: T.Text,
  content    :: T.Text,
  frontPage  :: Bool,
  createdAt  :: ZonedTime,
  prev       :: T.Text,
  next       :: T.Text
} deriving Show

data ArticleMigration = ArticleMigration {
  referenceM :: T.Text,
  titleM     :: T.Text,
  summaryM   :: T.Text,
  contentM   :: T.Text,
  frontPageM :: Bool,
  createdAtM :: ZonedTime
} deriving Show

data StoredFile = StoredFile {
  fileName     :: T.Text,
  fileData     :: ByteString,
  contentType  :: T.Text
} deriving Show

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: ArticleMigration
    dbName: article
    keys:
      - name: reference_uniq
    constructors:
      - name: ArticleMigration
        fields:
          - name: referenceM
            dbName: reference
          - name: titleM
            dbName: title
          - name: summaryM
            dbName: summary
          - name: contentM
            dbName: content
          - name: createdAtM
            dbName: created_at
            default: "now()"
          - name: frontPageM
            dbName: front_page
            default: "true"
        uniques:
          - name: reference_uniq
            fields: [referenceM]
  - entity: Article
    dbName: navigation
    constructors:
      - name: Article
        fields:
          - name: createdAt
            dbName: created_at
          - name: frontPage
            dbName: front_page
  - entity: StoredFile
    dbName: files
    constructors:
      - name: StoredFile
        fields:
          - name: fileData
            dbName: file_data
          - name: fileName
            dbName: file_name
          - name: contentType
            dbName: content_type
        uniques:
          - name: filename_uniq
            fields: [fileName]
|]

articleSplices :: MonadSnap n => Splices (RuntimeSplice n Article -> C.Splice n)
articleSplices = mapV (C.pureSplice . C.textSplice) $ do
  "articleReference" ## reference
  "articleTitle"     ## title
  "articleSummary"   ## summary
  "articleContent"   ## markdownToHtml . content
  "articleCreation"  ## presentTime . createdAt
  "articleRss"       ## rssTime . createdAt
  "articlePrev"      ## prev
  "articleNext"      ## next
        
allArticlesSplice :: C.Splice (AppHandler)
allArticlesSplice = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ getAllArticles

getAllArticles :: AppHandler [Article]
getAllArticles = do
  results <- runGH $ select $ (FrontPageField ==. True) `orderBy` [Desc CreatedAtField]
  return results

getLatestArticle :: AppHandler [Article]
getLatestArticle = do
  results <- runGH $ select $ (FrontPageField ==. True) `orderBy` [Desc CreatedAtField] `limitTo` 1
  return results

getFileFromDatabase :: String -> AppHandler StoredFile
getFileFromDatabase n = do
  results <- runGH $ select $ (FileNameField ==. (T.pack n)) `limitTo` 1
  return $ head results

getFile :: AppHandler ()
getFile = do
  param <- getParam "name"
  f <- getFileFromDatabase $ B.unpack $ fromMaybe "" param
  modifyResponse $ setContentType $ B.pack $ T.unpack $ contentType $ f
  writeBS $ fileData $ f

getSingleArticle :: String -> AppHandler Article
getSingleArticle k = do
  results <- runGH $ select $ (ReferenceField ==. (T.pack k)) `limitTo` 1
  return $ head results
  
articlesSplice :: Splices (C.Splice (AppHandler))
articlesSplice = "articles" ## allArticlesSplice

articleSpliceByReference :: C.Splice (AppHandler)
articleSpliceByReference = do
  promise <- C.newEmptyPromise
  outputChildren <- C.manyWithSplices C.runChildren articleSplices (C.getPromise promise)
  return $ C.yieldRuntime $ do
    ref <- lift $ getParam "reference"
    articles <- lift $ getSingleArticle $ B.unpack $ fromMaybe "" ref
    C.putPromise promise [articles] >> C.codeGen outputChildren

articleSplice :: Splices (C.Splice (AppHandler))
articleSplice = "article" ## articleSpliceByReference

latestSplices :: C.Splice (AppHandler)
latestSplices = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ getLatestArticle
  
latestSplice :: Splices (C.Splice (AppHandler))
latestSplice = "latest" ## latestSplices

markdownToHtml :: T.Text -> T.Text
markdownToHtml = pandocToHtml . markdownToPandoc

markdownToPandoc :: T.Text -> Pandoc
markdownToPandoc = handleError . readMarkdown def . T.unpack

pandocToHtml :: Pandoc -> T.Text
pandocToHtml = T.pack . writeHtmlString def

presentTime :: ZonedTime -> T.Text
presentTime = T.pack . formatTime defaultTimeLocale "%B %d, %Y"

rssTime :: ZonedTime -> T.Text
rssTime t = T.pack $ (formatTime defaultTimeLocale "%a, %d %b %0Y %H:%M:%S %z" t)

allCompiledSplices :: Splices (C.Splice (AppHandler))
allCompiledSplices = mconcat [ articlesSplice, articleSplice, latestSplice ]

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", ifTop $ cRender "index")
         , ("/articles/:reference", cRender "article")
         , ("/pages/:reference", cRender "page")
         , ("/static", serveDirectory "static")
         , ("/media", serveDirectory "media")
         , ("/rss", cRenderAs "application/rss+xml" "rss")
         , ("/sass", with sass sassServe)
         , ("/file/:name", getFile)
         , ("/favicon.ico", serveFile "static/favicon.ico")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Pure nonsense." Nothing $ do
  let hc = emptyHeistConfig
           & hcNamespace .~ ""
           & hcTemplateLocations .~ [loadTemplates "templates"]
           & hcLoadTimeSplices .~ defaultLoadTimeSplices
           & hcCompiledSplices .~ allCompiledSplices
  h <- nestSnaplet "" heist $ heistInit' "templates" hc
  n <- nestSnaplet "sass" sass initSass
  s <- nestSnaplet "sess" sess $
    initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  addRoutes routes
  
  cfg <- C.subconfig "postgresql-simple" <$> getSnapletUserConfig
  
  connstr <- liftIO $ decodeUtf8 <$> getConnectionString cfg
  p <- liftIO $ withPostgresqlPool (T.unpack connstr) 3 return
  liftIO $ print connstr
  liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) p)
  
  wrapSite (\site -> site <|> serveFile ("static/404.html"))
  return $ App h s p n

migrateDB :: (MonadIO m, PersistBackend m) => m ()
migrateDB = runMigration $ do
  G.migrate (undefined :: ArticleMigration)
  G.migrate (undefined :: StoredFile)
