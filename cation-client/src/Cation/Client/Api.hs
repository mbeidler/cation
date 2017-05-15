{-# LANGUAGE OverloadedStrings #-}
module Cation.Client.Api
  ( Response
  , cfg
  , onResp
  , req
  ) where

import           Cation.Common.Api         (API)
import           Data.Proxy                (Proxy)
import           React.Flux.Addons.Servant
import           Servant.API               (IsElem)

-- | A 'Response' is either a non-2XX status code with message or an 'a'
type Response a = Either (Int, String) a

-- | The request settings for 'API', consisting of base URL and timeout.
cfg :: ApiRequestConfig API
cfg =
  ApiRequestConfig "http://localhost:8081" (TimeoutMilliseconds (120 * 1000))

-- | A 'request' using 'cfg'.
req :: (IsElem endpoint API, HasAjaxRequest endpoint)
    => Proxy endpoint
    -> MkRequest endpoint
req = request cfg

-- | The default response handler. HTTP error status codes are written to the
-- console, along with the response message.
onResp :: (a -> IO b) -- ^ The function to run on success.
       -> Response a  -- ^ The response to process.
       -> b           -- ^ The state to return on failure.
       -> IO b        -- ^ The new state.
onResp onSuccess (Right x)  _  = onSuccess x
onResp _         (Left err) st = print err *> pure st
