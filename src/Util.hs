module Util (indent) where

import Data.Text qualified as T

import Prelude


indent :: Text -> Text
indent = T.unlines . fmap ("  " <>) . T.lines
