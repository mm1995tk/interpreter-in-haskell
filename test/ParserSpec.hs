{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ParserSpec where

import AST (Statement (ident))
import qualified AST
import Data.Text (Text)
import Parser (parse, parseCall, parseFn, parseIdent, parseLiteral, parsePrefixExpr)
import qualified Parser.Error as ParserError
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (EF, elabel, err, errFancy, fancy, shouldFailWith, shouldParse, utok, utoks)
import Text.Megaparsec (ErrorFancy (ErrorCustom))
import Text.Megaparsec.Error.Builder (ET)

spec_parse_literal :: Spec
spec_parse_literal = do
  describe "booleanのパース" $ do
    it "true 空白なし" $
      parse parseLiteral "true" `shouldParse` AST.LiteralExpr (AST.BoolLiteral True)
    it "true 空白あり" $
      parse parseLiteral "true  " `shouldParse` AST.LiteralExpr (AST.BoolLiteral True)
    it "false" $
      parse parseLiteral "false" `shouldParse` AST.LiteralExpr (AST.BoolLiteral False)
    it "falseのtypo `falsee`" $
      parse parseLiteral "falsee " `shouldFailWith` err 5 (literalUtoks "e")
    it "falseのtypo `ffalse`" $
      parse parseLiteral "ffalse" `shouldFailWith` err 0 (literalUtoks "ffals")
    it "適当な文字列" $
      parse parseLiteral "abcdeg" `shouldFailWith` err 0 (literalUtoks "abcde")
  describe "数値のパース" $ do
    it "数値" $
      parse parseLiteral "19" `shouldParse` AST.LiteralExpr (AST.NumLiteral 19)
    it "文字列がくっついている" $
      parse parseLiteral "12a" `shouldFailWith` err 2 (utok 'a' <> elabel "digit")
  describe "nullのパース" $ do
    it "正常" $
      parse parseLiteral "null " `shouldParse` AST.LiteralExpr AST.Null
    it "文字中に\"null\"を含むident `nullable`" $
      parse parseLiteral "nullable" `shouldFailWith` err 4 (literalUtoks "a")
  where
    literalUtoks :: Text -> ET Text
    literalUtoks t = utoks t <> elabel "literals"

spec_parse_ident :: Spec
spec_parse_ident = do
  it "正常" $
    parse parseIdent "callFunc" `shouldParse` AST.Identifier "callFunc"
  it "数値が含まれている" $
    parse parseIdent "callFunc1" `shouldParse` AST.Identifier "callFunc1"
  it "先頭が数値" $
    parse parseIdent "12a" `shouldFailWith` err 0 (utok '1' <> elabel "letter")

spec_parse_prefix_expr :: Spec
spec_parse_prefix_expr = do
  it "正常" $
    parse parsePrefixExpr "-1"
      `shouldParse` AST.PrefixExpr
        { prefixOp = AST.MinusPrefix
        , expr = AST.LiteralExpr $ AST.NumLiteral 1
        }
  it "前置演算子と式の間にスペースがある場合" $
    parse parsePrefixExpr "- 1  "
      `shouldParse` AST.PrefixExpr
        { prefixOp = AST.MinusPrefix
        , expr = AST.LiteralExpr $ AST.NumLiteral 1
        }

spec_parse_fn_expr :: Spec
spec_parse_fn_expr = do
  it "正常" $
    parse parseFn "fn( x , y   , ) {  let nullable = null ; let x = 1; return x;}"
      `shouldParse` AST.FnExpr
        { params = [AST.Identifier "x", AST.Identifier "y"]
        , body =
            [ AST.Let
                { ident = AST.Identifier "nullable"
                , expr = AST.LiteralExpr AST.Null
                }
            , AST.Let
                { ident = AST.Identifier "x"
                , expr = AST.LiteralExpr $ AST.NumLiteral 1
                }
            , AST.Return . AST.IdentExpr $ AST.Identifier "x"
            ]
        }

spec_parse_call_expr :: Spec
spec_parse_call_expr = do
  it "正常" $
    parse parseCall "func(x, plus(x,y))"
      `shouldParse` AST.CallExpr
        { called = AST.IdentExpr $ AST.Identifier "func"
        , args =
            [ AST.IdentExpr $ AST.Identifier "x"
            , AST.CallExpr
                { called = AST.IdentExpr $ AST.Identifier "plus"
                , args = [AST.IdentExpr $ AST.Identifier "x", AST.IdentExpr $ AST.Identifier "y"]
                }
            ]
        }
  it "呼び出し箇所がリテラル" $
    parse parseCall "1(x, plus(x,y))"
      `shouldFailWith` errFancy 1 (fancyErr $ ParserError.UnexpectedToken "expression that returns a function when evaluated" "literal")

fancyErr :: e -> EF e
fancyErr = fancy . ErrorCustom