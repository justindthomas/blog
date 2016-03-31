{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Lens
import           Snap
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _pg :: Snaplet Postgres 
    }

makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with pg get
    setLocalPostgresState s = local (set (pg . snapletValue) s)

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App


