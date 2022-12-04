module Parser.Error (Error (..), throwError) where

import qualified Data.Text as T
import qualified Text.Megaparsec as M
import Text.Printf (printf)

data Error
  = Panic
  | UnexpectedToken T.Text T.Text
  deriving (Eq, Show, Ord)

instance M.ShowErrorComponent Error where
  showErrorComponent Panic = "panic!"
  showErrorComponent (UnexpectedToken expected obtained) =
    printf "expected: %s, but got %s" expected obtained

throwError :: (M.Stream a) => Error -> M.Parsec Error a b
throwError = M.customFailure