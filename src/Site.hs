{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Database.PostgreSQL.Simple.FromRow
import           Data.Monoid
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Map.Syntax ((##), mapV)
import           Data.Maybe
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I
import qualified Heist.Compiled as C

------------------------------------------------------------------------------
import           Application

data Article = Article
  { title   :: T.Text
  , content :: T.Text
  }

runtime :: Monad n => RuntimeSplice n [Article]
runtime = return [
      Article "intro" "content",
      Article "intro2" "content2"
  ]

splicesFromArticle :: Monad n => Splices (RuntimeSplice n Article -> C.Splice n)
splicesFromArticle = mapS (C.pureSplice . C.textSplice) $ do
  "articleTitle" ## title
  "articleContent" ## content

renderArticles :: Monad n => RuntimeSplice n [Article] -> C.Splice n
renderArticles = C.manyWithSplices C.runChildren splicesFromArticle

articleSplices :: Monad n => Splices (C.Splice n)
articleSplices = "articles" ## (renderArticles runtime)

allCompiledSplices :: MonadSnap n => Splices (C.Splice n)
allCompiledSplices = mconcat [ articleSplices ]

indexHandler :: Handler App App ()
indexHandler = cRender "index"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("assets", serveDirectory "static")
         , ("/", indexHandler)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    let sc = mempty & scCompiledSplices .~ articleSplices
    addConfig h sc
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    d <- nestSnaplet "pg" pg pgsInit
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d
