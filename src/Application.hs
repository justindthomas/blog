{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------

import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Lens
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Sass
import           Data.Pool
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _gh    :: Pool Postgresql
    , _sass  :: Snaplet Sass
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance ConnectionManager App Postgresql where 
  withConn f app = withConn f (_gh app) 
  withConnNoTransaction f app = withConnNoTransaction f (_gh app) 
    
------------------------------------------------------------------------------
type AppHandler = Handler App App

runGH :: ConnectionManager b conn => DbPersist conn (NoLoggingT IO) a -> Handler b v a 
runGH f = withTop' id $ do 
    cm <- ask 
    liftIO $ runNoLoggingT (withConn (runDbPersist f) cm)

