module ParserSpec (spec_hspec) where

import Test.Hspec (Spec, it, shouldBe)

spec_hspec :: Spec
spec_hspec = do
  it "sample" $
    (1 :: Int) `shouldBe` 1

-- describe "boolのパース" $
--   it "is true" $
--     ( case parse parseBool "" (pack "true") of
--         Right v -> show v
--         Left _ -> show (AST.Bool False)
--     )
--       `shouldBe` "true"

-- describe "numberのパース" $
--   it "is number" $
--     ( case parse parseNumber "" (pack "123") of
--         Right v -> show v
--         Left _ -> show AST.Null
--     )
--       `shouldBe` "123"

-- describe "nullのパース" $
--   it "is null" $
--     ( case parse parseNull "" (pack "null") of
--         Right v -> show v
--         Left _ -> show $ AST.Number 0
--     )
--       `shouldBe` "null"

-- describe "symbolのパース" $
--   it "is symbol" $
--     ( case parse parseSymbol "" (pack "abc") of
--         Right v -> show v
--         Left _ -> show $ AST.Number 0
--     )
--       `shouldBe` "abc"

-- -- describe "let文のパース（数値のみ受理）" $
-- --   it "is let stmt" $
-- --     parseTest parseLetStmt (pack "let c = 3;")
-- --       `shouldReturn` ()
