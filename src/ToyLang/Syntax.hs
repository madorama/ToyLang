module ToyLang.Syntax where

import           Data.Text (Text)

data IdOrUnit
  = Id Text
  | Unit
  deriving (Show, Eq)

data Expr
  = EUnit
  | EInt Integer
  | EFloat Double
  | EList [Expr]
  | EVar Text
  | EString Text
  | EBinOp Text Expr Expr
  | ELet Text Expr
  | EApp Expr Expr
  | EFun IdOrUnit Expr
  | EBlock [Expr]
  | ERawJs Text
  deriving (Show, Eq)

type Program =
  [Expr]

mkFun :: [Text] -> Expr -> Expr
mkFun args body = case args of
  [] ->
    EFun Unit body

  [x] ->
    EFun (Id x) body

  x:xs ->
    EFun (Id x) (mkFun xs body)
