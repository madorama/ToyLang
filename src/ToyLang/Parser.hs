module ToyLang.Parser
  ( run
  ) where

import           Control.Monad.Combinators.Expr
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void
import           Text.Megaparsec                hiding (Label, unexpected)
import           Text.Megaparsec.Char           (eol)

import           Debug.Trace                    (traceShowId)
import           ToyLang.Syntax
import           ToyLang.Token

type Source =
  Text

run :: FilePath -> Source -> Either (ParseErrorBundle Text Void) [Expr]
run =
  runParser $ ws *> parseProgram <* ws <* eof

parseProgram :: Parser [Expr]
parseProgram =
  parseExprs

parseExprs :: Parser [Expr]
parseExprs =
  many parseExpr

parseExpr :: Parser Expr
parseExpr =
  let
    binary name = InfixL (EBinOp name <$ symbol name)
    table =
      [ [ binary "|>"
        ]
      , [ binary "+"
        , binary "-"
        ]
      , [ binary "*"
        , binary "/"
        ]
      ]
  in
  makeExprParser (ws *> parseSimpleExpr Nothing <* ws <* many (symbol ";" <|> eol)) table

parseSimpleExpr :: Maybe Expr -> Parser Expr
parseSimpleExpr = \case
  Just e ->
    choice
      [ parseApp e
          >>= parseSimpleExpr . Just
      , return e
      ]

  Nothing ->do
    e <-
      choice
        [ parseLet
        , parseFun
        , parseBlock
        , parseRawJs
        , parseLiteral
        , parens parseExpr
        ]

    parseSimpleExpr $ Just e

parseApp :: Expr -> Parser Expr
parseApp e = do
  choice
    [ try $ parens (EApp e <$> parseExpr)
    , EApp e <$> (EUnit <$ symbol "()")
    ]

parseVarName :: Parser Text
parseVarName =
  choice
    [ try $ do
        ident <- identifier
        _ <- symbol "."
        next <- parseVarName
        return $ T.concat [ident, ".", next]
    , try identifier
    ]

parseLiteral :: Parser Expr
parseLiteral =
  choice
    [ EFloat <$> try decimal
    , EInt <$> try integer
    , EString <$> try stringLiteral
    , EVar <$> try parseVarName
    , EUnit <$ string "()"
    , EList <$> brackets (commaSep parseExpr)
    ]

parseLet :: Parser Expr
parseLet =
  ELet
  <$> try (symbol "let" *> identifier)
  <*> try (symbol "=" *> parseExpr)

parseFun :: Parser Expr
parseFun =
  try $
    mkFun
    <$> try (symbol "(" *> commaSep identifier <* symbol ")")
    <*> try (symbol "=>" *> parseExpr)

parseBlock :: Parser Expr
parseBlock = do
  EBlock
  <$> try (symbol "{" *> optional eol *> parseExprs <* optional eol <* symbol "}")

parseRawJs :: Parser Expr
parseRawJs = do
  ERawJs
  <$> try (symbol "```" *> (T.pack <$> manyTill anySingle (symbol "```")))
