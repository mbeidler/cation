{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Cation.Server.Docs.Contacts where

import           Cation.Common.Api.Contacts
import           Control.Lens
import           Data.Aeson
import           Data.Proxy                 (Proxy)
import           Data.Swagger               hiding (Contact)
import           Servant
import           Servant.Swagger

-- | A Swagger endpoint documenting the Contacts API.
type ContactsSwagger = Contacts :> "swagger.json" :> Get '[JSON] Swagger

instance ToSchema CreateContact where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~
        "A contact is comprised of name, email, and phone"
    & mapped.schema.example ?~ toJSON
        (CreateContact "Christopher" "Walken" "christopher.walken@imdb.com"
           (Just "555-555-5555"))

instance ToSchema Contact where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~
        "A contact is comprised of name, email, and phone"
    & mapped.schema.example ?~ toJSON
        (Contact 42 "Christopher" "Walken" "christopher.walken@imdb.com"
           (Just "555-555-5555"))

-- | Describe the Contacts API.
contactsSwagger :: Swagger
contactsSwagger = toSwagger (Proxy :: Proxy ContactsAPI)
  & info.title   .~ "Contacts API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API for managing contacts."
  & info.license ?~
      ("BSD" & url ?~ URL "https://opensource.org/license/BSD-3-clause")
