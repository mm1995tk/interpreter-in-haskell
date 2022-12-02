module LibSpec (spec_hspec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Lib (plusOne)

spec_hspec :: Spec
spec_hspec = do
  describe "1 + 1 = 2" $
    it "書籍の実行例" $
      plusOne 1 `shouldBe` 2