{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving, AllowAmbiguousTypes #-}

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
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Time
import           Data.Time
import           Data.Time.Format
import           Data.ByteString (ByteString)
import qualified Data.Configurator as C
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.PostgresqlSimple
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
------------------------------------------------------------------------------
import           Application

data PGArticle = PGArticle {
  pg_id         :: Int,
  pg_reference  :: T.Text,
  pg_title      :: T.Text,
  pg_summary    :: T.Text,
  pg_content    :: T.Text,
  pg_created_at :: ZonedTime
} deriving Show

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

articleSplices :: Monad n => Splices (RuntimeSplice n PGArticle -> C.Splice n)
articleSplices = mapV (C.pureSplice . C.textSplice) $ do
        "articleReference" ## pg_reference
        "articleTitle"     ## pg_title
        "articleSummary"   ## pg_summary
        "articleContent"   ## markdownToHtml . pg_content
        "articleCreation"  ## presentTime . pg_created_at
        "articleRss"       ## rssTime . pg_created_at

allArticlesSplice :: (HasPostgres n, Monad n) => C.Splice n
allArticlesSplice = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ query_ "SELECT * FROM article ORDER BY created_at DESC"

articlesSplice :: (HasPostgres n, Monad n) => Splices (C.Splice n)
articlesSplice = "articles" ## allArticlesSplice

articleSpliceById :: (HasPostgres n, MonadSnap n) => C.Splice n
articleSpliceById = do
  promise <- C.newEmptyPromise
  outputChildren <- C.manyWithSplices C.runChildren articleSplices (C.getPromise promise)
  return $ C.yieldRuntime $ do
    ref <- lift $ getParam "reference"
    articles <- lift $ query "SELECT * FROM article WHERE reference = ?" (Only ref)
    C.putPromise promise articles >> C.codeGen outputChildren

articleSplice :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
articleSplice = "article" ## articleSpliceById

markdownToHtml :: T.Text -> T.Text
markdownToHtml = pandocToHtml . markdownToPandoc

markdownToPandoc :: T.Text -> Pandoc
markdownToPandoc = handleError . readMarkdown def . T.unpack

pandocToHtml :: Pandoc -> T.Text
pandocToHtml = T.pack . writeHtmlString def

presentTime :: ZonedTime -> T.Text
presentTime = T.pack . formatTime defaultTimeLocale "%B %d, %Y"

rssTime :: ZonedTime -> T.Text
rssTime t = T.pack $ (formatTime defaultTimeLocale rfc822DateFormat t)

allCompiledSplices :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
allCompiledSplices = mconcat [ articlesSplice, articleSplice ]

------------------------------------------------------------------------------
-- / Postgres

instance FromRow PGArticle where
    fromRow = PGArticle <$> field <*> field <*> field <*> field <*> field <*> field

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
    d <- nestSnaplet "pg" pg pgsInit
    addRoutes routes

    cfg <- getSnapletUserConfig
    cfg <- C.subconfig "postgresql-simple" <$> getSnapletUserConfig

    connstr <- liftIO $ decodeUtf8 <$> getConnectionString cfg
    p <- liftIO $ withPostgresqlPool (T.unpack connstr) 3 return
    liftIO $ print connstr
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) p)

    return $ App h s d

migrateDB :: (MonadIO m, PersistBackend m) => m ()
migrateDB = runMigration $ do
      G.migrate (undefined :: Article)
