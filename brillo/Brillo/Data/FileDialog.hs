module Brillo.Data.FileDialog (
  FileDialog (..),
  SelectionMode (..),
)
where

import Data.Text (Text)


data SelectionMode
  = SingleFileSelect
  | MultiFileSelect
  | SingleDirectorySelect
  deriving (Show, Eq)


{-| The 'FileDialog' represents a dialog
| that can be opened by the user to select files or directories.
-}
data FileDialog
  = FileDialog
  { title :: !Text
  -- ^ Title of the dialog
  , defaultPath :: !Text
  -- ^ Default path to open the dialog at
  , filterPatterns :: ![Text]
  -- ^ Filter patterns like `["*.jpg", "*.png"]`
  , filterDescription :: !Text
  -- ^ Filter description like `"text files"`
  , selectionMode :: !SelectionMode
  -- ^ Single or multiple selections
  }
  deriving (Show, Eq)
