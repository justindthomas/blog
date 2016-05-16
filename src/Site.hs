{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving, AllowAmbiguousTypes, FlexibleContexts #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Logger
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Configurator as C
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Compiled.LowLevel as C
import qualified Heist.Interpreted as I
import qualified Data.Text.Lazy as L
import           Text.Pandoc
import           Text.Pandoc.Error (handleError)
import           Control.Monad.IO.Class (liftIO)
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
  created_at :: ZonedTime
} deriving Show

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: Article
    dbName: article
    keys:
      - name: reference_uniq
    constructors:
      - name: Article
        fields:
          - name: created_at
            dbname: created_at
            default: "now()"
        uniques:
          - name: reference_uniq
            fields: [reference]
|]

articleSplices :: MonadSnap n => Splices (RuntimeSplice n Article -> C.Splice n)
articleSplices = mapV (C.pureSplice . C.textSplice) $ do
        "articleReference" ## reference
        "articleTitle"     ## title
        "articleSummary"   ## summary
        "articleContent"   ## markdownToHtml . content
        "articleCreation"  ## presentTime . created_at
        "articleRss"       ## rssTime . created_at
        
allArticlesSplice :: C.Splice (Handler App App)
allArticlesSplice = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ getAllArticles

getAllArticles :: Handler App App [Article]
getAllArticles = do
  results <- runGH $ select $ CondEmpty `orderBy` [Desc Created_atField]
  return results

getSingleArticle :: String -> Handler App App Article
getSingleArticle k = do
  results <- runGH $ select $ (ReferenceField ==. (T.pack k)) `limitTo` 1
  return $ head results

articlesSplice :: Splices (C.Splice (Handler App App))
articlesSplice = "articles" ## allArticlesSplice

articleSpliceByReference :: C.Splice (Handler App App)
articleSpliceByReference = do
  promise <- C.newEmptyPromise
  outputChildren <- C.manyWithSplices C.runChildren articleSplices (C.getPromise promise)
  return $ C.yieldRuntime $ do
    ref <- lift $ getParam "reference"
    articles <- lift $ getSingleArticle $ B.unpack $ fromMaybe "" ref
    C.putPromise promise [articles] >> C.codeGen outputChildren

articleSplice :: Splices (C.Splice (Handler App App))
articleSplice = "article" ## articleSpliceByReference

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

allCompiledSplices :: Splices (C.Splice (Handler App App))
allCompiledSplices = mconcat [ articlesSplice, articleSplice ]

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", ifTop $ cRender "index")
         , ("/articles/:reference", cRender "article")
         , ("/static", serveDirectory "static")
         , ("/media", serveDirectory "media")
         , ("/rss", cRenderAs "application/rss+xml" "rss")
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
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes

    cfg <- getSnapletUserConfig
    cfg <- C.subconfig "postgresql-simple" <$> getSnapletUserConfig

    connstr <- liftIO $ decodeUtf8 <$> getConnectionString cfg
    p <- liftIO $ withPostgresqlPool (T.unpack connstr) 3 return
    liftIO $ print connstr
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) p)

    return $ App h s p

migrateDB :: (MonadIO m, PersistBackend m) => m ()
migrateDB = runMigration $ do
      G.migrate (undefined :: Article)

