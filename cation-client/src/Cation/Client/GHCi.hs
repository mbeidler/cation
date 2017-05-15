{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Cation.Client.GHCi where

import Cation.Client.Components.Modal
import Data.JSString (JSString, toUpper)
import React.Flux

loadDeps :: IO ()
loadDeps = do
  js_addScript "http://localhost:8080/bundle.js"
  js_createRoot "app"

renderApp :: IO ()
renderApp = reactRender "app" view' ()
  where
    view' :: ReactView ()
    view' = defineView "app" $ const $ do
              h1_ "Hello, Test!"
              view nameForm mempty mempty
              view flavorForm mempty mempty
              button_ [ "type" $= "button"
                      , "className" $= "btn btn-primary pull-right flex-last"
                      , "data-toggle" $= "modal"
                      , "data-target" $= "#testModal" ] "Test Modal"
              modal "testModal" "Test" (h1_ "Welcome!")

nameForm :: ReactView ()
nameForm = defineStatefulView "nameForm" "" $
  \state args ->
      form_ [] $ do
        label_ [] $ do
          "Name: "
          input_ [ "type" $= "text"
                 , "value" $= state
                 , onChange $ \e _ ->
                     ([], Just (toUpper $ target e "value")) ]
        input_ [ "type" $= "submit", "value" $= "Submit" ]

flavorForm :: ReactView ()
flavorForm = defineStatefulView "flavorForm" "" $
  \state args ->
    form_ [] $ do
      label_ [] $ do
        "Pick your favorite La Croix flavor: "
        select_ [ "value" $= state
                , onChange $ \e _ -> ([], Just (target e "value")) ] $ do
          option_ [ "value" $= "grapefruit" ] "Grapefruit"
          option_ [ "value" $= "lime" ] "Lime"
          option_ [ "value" $= "coconut" ] "Coconut"
          option_ [ "value" $= "mango" ] "Mango"
      input_ [ "type" $= "submit"
             , "value" $= "Submit" ]

foreign import javascript unsafe
  "function addScript(src) {\
  \  var head = document.getElementsByTagName('head')[0];\
  \  var script = document.createElement('script');\
  \  script.type = 'text/javascript';\
  \  script.src = src;\
  \  head.appendChild(script);\
  \}addScript($1);"
  js_addScript :: JSString -> IO ()

foreign import javascript unsafe
  "function createRoot(name) {\
  \  var e = document.createElement('div');\
  \  e.id = name;\
  \  document.body.appendChild(e);\
  \}createRoot($1);"
  js_createRoot :: JSString -> IO ()
