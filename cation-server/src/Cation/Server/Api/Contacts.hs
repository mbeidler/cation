{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Cation.Server.Api.Contacts where

import           Cation.Common.Api.Contacts
import           Cation.Common.Conventions
import           Cation.Server.Base
import           Cation.Server.Db            (runDb)
import qualified Cation.Server.Db            as Db
import           Data.Maybe                  (maybe)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (Entity (..), delete, fromSqlKey, get,
                                              insert, replace, selectList, toSqlKey)
import           Servant

contactsServer :: ServerT ContactsAPI App
contactsServer = listContacts
            :<|> createContact
            :<|> updateContact
            :<|> getContact
            :<|> deleteContact

listContacts :: App [Contact]
listContacts = map fromEntity <$> runDb (selectList [] [])

createContact :: CreateContact -> App Contact
createContact CreateContact{..} = do
  newId <- runDb $ insert
    (Db.Contact cContactFirstName cContactLastName cContactEmail cContactPhone)
  return $
    Contact (fromSqlKey newId) cContactFirstName cContactLastName cContactEmail
      cContactPhone

updateContact :: Contact -> App Contact
updateContact c@Contact{..} = do
  runDb (replace (toSqlKey contactId) $
    Db.Contact contactFirstName contactLastName contactEmail contactPhone)
  return c

getContact :: Key -> App Contact
getContact id = do
  c <- runDb $ get (toSqlKey id)
  case c of
    Just c -> return $ fromModel id c
    Nothing -> throwError err404 { errBody = "contact not found" }

deleteContact :: Key -> App Key
deleteContact id = runDb (delete (toSqlKey id :: Db.ContactId)) *> pure id

fromEntity :: Entity Db.Contact -> Contact
fromEntity (Entity id c) = fromModel (fromSqlKey id) c

fromModel :: Key -> Db.Contact -> Contact
fromModel key Db.Contact{..} =
  Contact key contactFirstName contactLastName contactEmail contactPhone
