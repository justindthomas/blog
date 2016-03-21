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
import           Control.Monad.Trans
import           Database.PostgreSQL.Simple.FromRow
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application

data Article = Article
  { id      :: Int
  , title   :: T.Text
  , content :: T.Text
  }

instance FromRow Article where
  fromRow = Article <$> field <*> field <*> field

instance Show Article where
  show (Article id title content) =
    "Article { title: " ++ T.unpack title ++ ", content: " ++ T.unpack content ++ " }n"

-- | Handle request for posts
handlePosts :: Handler App App ()
handlePosts = do
  posts <- with pg $ query_ "select * from article"
  liftIO $ print (posts :: [Article])

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/posts", handlePosts)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
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

