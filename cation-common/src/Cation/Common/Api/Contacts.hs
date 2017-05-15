{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Cation.Common.Api.Contacts where

import           Cation.Common.Api.Combinators
import           Cation.Common.Conventions     (Key, jsonOpts)
import           Control.DeepSeq               (NFData)
import           Data.Aeson.TH                 (deriveJSON)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Servant.API

data CreateContact = CreateContact
  { cContactFirstName :: Text
  , cContactLastName  :: Text
  , cContactEmail     :: Text
  , cContactPhone     :: Maybe Text
  } deriving (Generic, NFData)

$(deriveJSON (jsonOpts 8) ''CreateContact)

data Contact = Contact
  { contactId        :: Key
  , contactFirstName :: Text
  , contactLastName  :: Text
  , contactEmail     :: Text
  , contactPhone     :: Maybe Text
  } deriving (Generic, NFData)

$(deriveJSON (jsonOpts 7) ''Contact)

type Contacts = "contacts"

type GetContacts   = GetR     Contacts Contact
type PostContact   = PostR    Contacts CreateContact Contact
type PutContact    = PutR     Contacts Contact
type GetContact    = GetByIdR Contacts "id" Contact
type DeleteContact = DeleteR  Contacts "id"

type ContactsAPI = GetContacts
              :<|> PostContact
              :<|> PutContact
              :<|> GetContact
              :<|> DeleteContact
