{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Cation.Client.Components.Contacts.Store where

import           Cation.Client.Api          (Response, cfg, onResp, req)
import           Cation.Common.Api.Contacts
import           Control.DeepSeq            (NFData)
import           Data.Proxy                 (Proxy (..))
import           GHC.Generics               (Generic)
import           React.Flux
import           React.Flux.Addons.Servant  (request)

data ContactsStore
  = Init
  | ContactsState { contacts :: [Contact] }
  deriving (Generic, NFData)

data ContactsAction
  = Load
  | LoadComplete (Response [Contact])
  deriving (Generic, NFData)

instance StoreData ContactsStore where
  type StoreAction ContactsStore = ContactsAction

  transform action state =
    case action of
      Load -> do
        req (Proxy :: Proxy GetContacts) (dispatch . LoadComplete)
        return state
      LoadComplete response ->
        onResp (pure . ContactsState) response state

dispatch :: ContactsAction -> IO [SomeStoreAction]
dispatch action = pure [SomeStoreAction contactsStore action]

contactsStore :: ReactStore ContactsStore
contactsStore = mkStore Init
