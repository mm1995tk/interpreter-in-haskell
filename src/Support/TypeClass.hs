module Support.TypeClass (Display (..)) where

class Show a => Display a where
  display :: a -> String
  display = show
