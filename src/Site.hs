{-# LANGUAGE OverloadedStrings #-}

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
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Time
import           Data.Time
import           Data.Time.Format
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import qualified Heist.Compiled as C
import qualified Data.Text.Lazy as L
import           Text.Pandoc
import           Text.Pandoc.Error (handleError)

------------------------------------------------------------------------------
import           Application

data Article = Article
  { articleId  :: Int
  , title      :: T.Text
  , content    :: T.Text
  , created_at :: LocalTime
  }

allArticlesSplice :: (HasPostgres n, Monad n) => C.Splice n
allArticlesSplice = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ query_ "SELECT * FROM article ORDER BY created_at DESC"
  where
    articleSplices = do
      mapV (C.pureSplice . C.textSplice) $ do
        "articleId" ## T.pack . show . articleId
        "articleTitle" ## title
        "articleContent" ## markdownToHtml . content
        "articleCreation" ## presentTime . created_at

singleArticleSplice :: (MonadSnap n, HasPostgres n) => C.Splice n
singleArticleSplice = do
  a <- getParam "id"
  C.manyWithSplices C.runChildren articleSplice $
    lift $ query "SELECT * FROM article WHERE id = ?" (Only a)
  where
    articleSplice = do
      mapV (C.pureSplice . C.textSplice) $ do
        "articleId" ## T.pack . show . articleId
        "articleTitle" ## title
        "articleContent" ## markdownToHtml . content
        "articleCreation" ## presentTime . created_at

markdownToHtml :: T.Text -> T.Text
markdownToHtml = pandocToHtml . markdownToPandoc

markdownToPandoc :: T.Text -> Pandoc
markdownToPandoc = handleError . readMarkdown def . T.unpack

pandocToHtml :: Pandoc -> T.Text
pandocToHtml = T.pack . writeHtmlString def

presentTime :: LocalTime -> T.Text
presentTime = T.pack . formatTime defaultTimeLocale "%B %d, %Y"

articlesSplice :: (HasPostgres n, Monad n) => Splices (C.Splice n)
articlesSplice = "articles" ## allArticlesSplice

articleSplice :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
articleSplice = "article" ## singleArticleSplice

allCompiledSplices :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
allCompiledSplices = mconcat [ articlesSplice, articleSplice ]

getArticle :: Handler App App ()
getArticle = do
  articleId <- getParam "id"
  articles <- query "SELECT * FROM article WHERE id = ?" (Only articleId)
  writeBS $ encodeUtf8 $ markdownToHtml $ content $ head articles

------------------------------------------------------------------------------
-- / Postgres

instance FromRow Article where
    fromRow = Article <$> field <*> field <*> field <*> field

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", ifTop $ cRender "index")
         , ("/articles/:id", getArticle)
         , ("/static", serveDirectory "static")
         , ("/media", serveDirectory "media")
         , ("/rss", cRenderAs "application/rss+xml" "rss")
         , ("/favicon.ico", serveFile "static/favicon.ico")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Pure nonsense." Nothing $ do
    tz <- liftIO getCurrentTimeZone
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
    return $ App h s d
