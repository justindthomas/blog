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
  }

splice :: (HasPostgres n, Monad n) => C.Splice n
splice = do
  C.manyWithSplices C.runChildren articleSplices $
    lift $ query_ "SELECT * FROM article"
  where
    articleSplices = do
      mapV (C.pureSplice . C.textSplice) $ do
        "articleId" ## T.pack . show . articleId
        "articleTitle" ## title
        "articleContent" ## markdownToHtml . content

markdownToHtml :: T.Text -> T.Text
markdownToHtml = T.pack . L.unpack . X.renderHtml . MD.markdown MD.def . L.fromStrict

articlesSplice :: (HasPostgres n, Monad n) => Splices (C.Splice n)
articlesSplice = "articles" ## splice

allCompiledSplices :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
allCompiledSplices = mconcat [ articlesSplice ]

------------------------------------------------------------------------------
-- / Postgres

instance FromRow Article where
    fromRow = Article <$> field <*> field <*> field

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", ifTop $ cRender "index")
         , ("/static", serveDirectory "static")
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
    return $ App h s d
