module Cation.Common.Conventions
  ( jsonOpts
  , Key
  ) where

import           Data.Aeson.TH (Options (..), defaultOptions)
import           Data.Char     (isLower, toLower)
import           GHC.Int       (Int64)

-- | Our default identifier type.
type Key = Int64

-- | Our default JSON serialization conventions.
jsonOpts :: Int -> Options
jsonOpts prefixLength =
  defaultOptions { fieldLabelModifier = camelCase . drop prefixLength
                 , omitNothingFields = True }

-- | 'camelCase' a PascalCased string.
camelCase :: String -> String
camelCase (c:cs) = toLower c : cs
camelCase []     = []
