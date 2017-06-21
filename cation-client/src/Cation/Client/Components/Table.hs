{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Cation.Client.Components.Table where

import           Data.Aeson    (Value (..))
import           Data.Default  (Default (..))
import           Data.JSString (JSString)
import           Data.Text     (Text)
import           Data.Vector   (fromList)
import           React.Flux

data TableCfg a handler = TableCfg
  { tblClassName     :: JSString
  , tblSortable      :: Bool
  , tblItemsPerPage  :: Int
  , tblPageButtonMax :: Int
  , tblColumns       :: [ColumnCfg a handler]
  }

instance Default (TableCfg a handler) where
  def =
    TableCfg "table table-striped tabled-bordered table-hover table-sm"
      True 15 5 []

data ColumnCfg a handler = ColumnCfg
  { colName       :: Text
  , colTitle      :: Text
  , colFilterable :: Bool
  , colSelector   :: a -> ReactElementM handler ()
  }

instance Default (ColumnCfg a handler) where
  def = ColumnCfg mempty mempty True (const mempty)

-- | Define a column.
defCol :: Text                            -- ^ The column header text.
       -> (a -> ReactElementM handler ()) -- ^ A renderer for column data.
       -> ColumnCfg a handler             -- ^ The column configuration.
defCol name = ColumnCfg name name True

-- | Render a Reactable table using the provided configuration and source list.
table_ :: TableCfg a handler -> [a] -> ReactElementM handler ()
table_ TableCfg{..} xs =
  foreign_ "Table"
    [ "className" $= tblClassName
    , "sortable" @= Bool tblSortable
    , "itemsPerPage" @= tblItemsPerPage
    , "pageButtonLimit" @= tblPageButtonMax
    , "filterable" @= Array (fromList filterableColumns)
    ] $ do
    foreign_ "Thead" [] $ mapM_ rowHeader tblColumns
    mapM_ (rowValue tblColumns) xs
  where
    rowHeader ColumnCfg{..} =
      foreign_ "Th" [ "column" &= colName ] (elemText colName)
    rowValue cs x =
      foreign_ "Tr" [] $
        mapM_ (\ColumnCfg{..} ->
          foreign_ "Td" [ "column" &= colName ] (colSelector x)) cs
    filterableColumns =
      map (String . colName) (filter colFilterable tblColumns)
