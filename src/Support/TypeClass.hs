{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Support.TypeClass where

import Data.Text (Text, unpack)

class Show a => Display a where
  display :: a -> String
  display = unpack . displayText

  displayText :: a -> Text
