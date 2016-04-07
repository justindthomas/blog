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
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import qualified Heist.Compiled as C
import qualified Text.Markdown as MD
import qualified Text.Blaze.Html.Renderer.Text as X
import           Text.Blaze.Html
import qualified Data.Text.Lazy as L

------------------------------------------------------------------------------
import           Application

data Article = Article
  { articleId  :: Int
  , title      :: T.Text
  , content    :: T.Text
  , created_at :: LocalTime
  }

splice :: (HasPostgres n, Monad n) => TimeZone -> C.Splice n
splice tz = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ query_ "SELECT * FROM article ORDER BY created_at DESC"
  where
    articleSplices = do
      mapV (C.pureSplice . C.textSplice) $ do
        "articleId" ## T.pack . show . articleId
        "articleTitle" ## title
        "articleContent" ## markdownToHtml . content
        "articleCreation" ## presentTime . created_at

markdownToHtml :: T.Text -> T.Text
markdownToHtml = T.pack . L.unpack . X.renderHtml . MD.markdown MD.def . L.fromStrict

presentTime :: LocalTime -> T.Text
presentTime = T.pack . formatTime defaultTimeLocale "%B %d, %Y"

articlesSplice :: (HasPostgres n, Monad n) => TimeZone -> Splices (C.Splice n)
articlesSplice tz = "articles" ## splice tz

--allCompiledSplices :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
--allCompiledSplices = mconcat [ articlesSplice ]

------------------------------------------------------------------------------
-- / Postgres

instance FromRow Article where
    fromRow = Article <$> field <*> field <*> field <*> field

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", ifTop $ cRender "index")
         , ("/static", serveDirectory "static")
         , ("/media", serveDirectory "media")
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
             & hcCompiledSplices .~ (articlesSplice tz)
    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "pg" pg pgsInit
    addRoutes routes
    return $ App h s d
