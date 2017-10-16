{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Cation.Server.Api.Contacts where

import           Cation.Common.Api.Contacts
import           Cation.Common.Conventions
import           Cation.Server.Base
import           Cation.Server.Db            (runDb)
import qualified Cation.Server.Db            as Db
import           Database.Persist.Postgresql (Entity (..), delete, fromSqlKey,
                                              get, insert, replace, selectList,
                                              toSqlKey)
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
getContact key = do
  c <- runDb $ get (toSqlKey key)
  case c of
    Just contact -> return $ fromModel key contact
    Nothing      -> throwError err404 { errBody = "contact not found" }

deleteContact :: Key -> App Key
deleteContact key = runDb (delete (toSqlKey key :: Db.ContactId)) *> pure key

fromEntity :: Entity Db.Contact -> Contact
fromEntity (Entity key c) = fromModel (fromSqlKey key) c

fromModel :: Key -> Db.Contact -> Contact
fromModel key Db.Contact{..} =
  Contact key contactFirstName contactLastName contactEmail contactPhone
