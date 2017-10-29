{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cation.Server where

import           Cation.Common.Api.Contacts
import           Cation.Server.Api.Contacts
import           Cation.Server.Base
import           Cation.Server.Db            (doMigrations)
import           Cation.Server.Docs.Contacts
import           Control.Category            ((<<<), (>>>))
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Database.Persist.Sql        (runSqlPool)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Safe                        (readMay)
import           Servant
import           System.Environment          (lookupEnv)

-- | Given a 'Config', a WAI 'Application' is returned, which any WAI compliant
-- server can run.
app :: Config -> Application
app cfg = serve (Proxy :: Proxy API) (appToServer cfg)

-- | Our entire API.
type API = ContactsSwagger :<|> ContactsAPI

-- | Our servers combined with the Swagger documentation endpoints.
server :: ServerT API App
server = return contactsSwagger :<|> contactsServer

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server API
appToServer cfg = enter (convertApp cfg >>> NT Handler) server

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = runReaderTNat cfg <<< NT runApp

-- | The 'run' function gathers the required environment information and
-- runs the application.
runServer :: IO ()
runServer = do
  env  <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8081
  pool <- makePool env
  let cfg = Config { getPool = pool, getEnv = env }
      logger = setLogger env
  runSqlPool doMigrations pool
  run port $ logger $ simpleCors $ app cfg

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $ mconcat
        [ "Failed to read [["
        , str
        , "]] for environment variable "
        , env
        ]
