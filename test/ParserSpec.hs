{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ParserSpec where

import qualified AST
import Data.Text (Text)
import Parser (parse, parseBool, parseIdent, parseNumber)

import Test.Hspec (Spec, it)
import Test.Hspec.Megaparsec (elabel, err, etoks, shouldFailWith, shouldParse, utok, utoks)
import Text.Megaparsec.Error.Builder (ET)

spec_parse_bool :: Spec
spec_parse_bool = do
  it "true 空白なし" $
    parse parseBool "true" `shouldParse` AST.BoolLiteral True
  it "true 空白あり" $
    parse parseBool "true  " `shouldParse` AST.BoolLiteral True
  it "false" $
    parse parseBool "false" `shouldParse` AST.BoolLiteral False
  it "falseのtypo `falsee`" $
    parse parseBool "falsee " `shouldFailWith` err 5 (utok 'e')
  it "falseのtypo `ffalse`" $
    parse parseBool "ffalse" `shouldFailWith` err 0 (boolUtoks "ffals")
  it "適当な文字列" $
    parse parseBool "abcdeg" `shouldFailWith` err 0 (boolUtoks "abcde")
 where
  boolUtoks :: Text -> ET Text
  boolUtoks t = utoks t <> etoks "false" <> etoks "true"

spec_parse_number :: Spec
spec_parse_number = do
  it "数値" $
    parse parseNumber "19" `shouldParse` AST.NumLiteral 19
  it "文字列がくっついている" $
    parse parseNumber "12a" `shouldFailWith` err 2 (utok 'a' <> elabel "digit")

spec_parse_ident :: Spec
spec_parse_ident = do
  it "正常" $
    parse parseIdent "callFunc" `shouldParse` AST.Identifier "callFunc"
  it "数値が含まれている" $
    parse parseIdent "callFunc1" `shouldParse` AST.Identifier "callFunc1"
  it "先頭が数値" $
    parse parseIdent "12a" `shouldFailWith` err 0 (utok '1' <> elabel "letter")

-- describe "nullのパース" $
--   it "is null" $
--     ( case parse parseNull "" (pack "null") of
--         Right v -> show v
--         Left _ -> show $ AST.Number 0
--     )
--       `shouldBe` "null"
