module Js.CodeGen
  ( gen
  ) where

import           Prettyprinter

import           Madlib.Operator
import           Madlib.Pretty

import           Js.Syntax

gen :: Program -> Doc ann
gen p =
  join line (map ppAstBlock p)

ppAstBlock :: Ast -> Doc ann
ppAstBlock = \case
  AExpr e@ERawJs{} ->
    ppExpr e

  AExpr e ->
    ppExpr e
    |> add ";"

  ast ->
    ppAst ast

ppAst :: Ast -> Doc ann
ppAst = \case
  AExpr e ->
    ppExpr e

  AConst name e ->
    "const "
    |> addPretty name
    |> add " = "
    |> add (ppExpr e)
    |> add
        ( case e of
            ELambda{} ->
              ""

            _ ->
              ";"
        )

  AReturn me ->
    "return "
    |> add
        ( ifMaybe
            (\e ->
                "("
                |> add (ppExpr e)
                |> add ")"
            )
            me
        )
    |> add ";"

  ABlock asts ->
    "{"
    |> addNest 2
        ( line
          |> add (vsep (map ppAstBlock asts))
        )
    |> add line
    |> add "}"


ppExpr :: Expr -> Doc ann
ppExpr = \case
  EIdent name ->
    pretty name

  EInt n ->
    pretty n

  EFloat f ->
    pretty f

  EString s ->
    "\""
    |> addPretty s
    |> add "\""

  EList xs ->
    "["
    |> add (join "," (map ppExpr xs))
    |> add "]"

  EBinOp op l r ->
    ppExpr l
    |> addPretty op
    |> add (ppExpr r)

  ELambda args ast ->
    "("
    |> add (join "," $ map pretty args)
    |> add ") => "
    |> add (ppAst ast)

  ECall l ps ->
    ppExpr l
    |> add "("
    |> add (join "," $ map ppExpr ps)
    |> add ")"

  ERawJs js ->
    pretty js
