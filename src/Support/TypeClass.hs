{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Support.TypeClass where

class Show a => Display a where
  display :: a -> String
  display = show
