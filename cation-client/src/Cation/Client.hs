{-# LANGUAGE OverloadedStrings #-}
module Cation.Client where

import           Cation.Client.Components.Contacts.Store
import           Cation.Client.Components.Contacts.View
import           React.Flux
import           React.Flux.Ajax

app :: IO ()
app = do
  initAjax
  alterStore contactsStore LoadContacts
  reactRender "app" contactsApp ()
