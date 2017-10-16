{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Cation.Server.Base where

import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.Logger                 (runStdoutLoggingT)

import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

-- The code in this module was modified from the servant-persistent repository.

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype App a
  = App { runApp :: ReaderT Config (ExceptT ServantErr IO) a
        } deriving ( Applicative, Functor, Monad, MonadReader Config
                   , MonadError ServantErr, MonadIO )

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { getPool :: ConnectionPool
  , getEnv  :: Environment
  }

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment = Development
                 | Test
                 | Production
                 deriving (Eq, Read, Show)

-- | Return the logging 'Middleware' to use for a given environment.
setLogger :: Environment -> Middleware
setLogger Production  = logStdout
setLogger Test        = logStdout
setLogger Development = logStdoutDev

-- | The number of connections to pool for a given environment.
envPool :: Environment -> Int
envPool Production  = 8
envPool Test        = 1
envPool Development = 1

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> IO ConnectionPool
makePool Production = do
  pool <- runMaybeT $ do
    let keys = [ "host="
               , "port="
               , "user="
               , "password="
               , "dbname="
               ]
        envs = [ "PGHOST"
               , "PGPORT"
               , "PGUSER"
               , "PGPASS"
               , "PGDATABASE"
               ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = mconcat . zipWith (<>) keys $ BS.pack <$> envVars
    runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
  case pool of
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
    Nothing ->
      throwIO (userError
        "Database Configuration not present in environment.")
    Just a -> return a
makePool Test =
  runStdoutLoggingT (createPostgresqlPool (connStr "test") (envPool Test))
makePool Development =
  runStdoutLoggingT (createPostgresqlPool (connStr "") (envPool Development))

-- | A basic 'ConnectionString' for local/test development. Pass in either @""@
-- for 'Development' or @"test"@ for 'Test. 'Production' connection strings are
-- acquired from environment variables.
connStr :: BS.ByteString -> ConnectionString
connStr env =
  "host=localhost dbname=cation" <> env <> " user=test password=test port=5432"
