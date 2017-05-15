{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
module Cation.Common.Api.Combinators where

import           Cation.Common.Conventions  (Key)
import           GHC.TypeLits               (Symbol)
import           Servant.API

type GetR (path :: Symbol) resource
  = path :> Get '[JSON] [resource]

type PostR (path :: Symbol) createType resource
  = path :> ReqBody '[JSON] createType :> Post '[JSON] resource

type PutR (path :: Symbol) resource
  = path :> ReqBody '[JSON] resource :> Put '[JSON] resource

type GetByIdR (path :: Symbol) (capture :: Symbol) resource
  = path :> Capture capture Key :> Get '[JSON] resource

type DeleteR (path :: Symbol) (capture :: Symbol)
  = path :> Capture capture Key :> Delete '[JSON] Key
