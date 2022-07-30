module Js.Syntax where

import           Data.Text (Text)

data Ast
  = AExpr Expr
  | AConst Text Expr
  | AReturn (Maybe Expr)
  | ABlock [Ast]
  deriving (Eq, Show)

data Expr
  = EIdent Text
  | EInt Integer
  | EFloat Double
  | EString Text
  | EBinOp Text Expr Expr
  | EList [Expr]
  | ELambda [Text] Ast
  | ECall Expr [Expr]
  | ERawJs Text
  deriving (Eq, Show)

type Program =
  [Ast]
