module ToyLang.Compile where

import           Control.Arrow
import           Data.List
import           Data.Text       (Text)

import           Madlib.Operator

import qualified Js.CodeGen      as Js
import qualified Js.Syntax       as Js

import qualified ToyLang.Parser  as Parser
import           ToyLang.Syntax

mkBuiltinName :: Text -> Text
mkBuiltinName name =
  "___builtin$" <> name

return_ :: Js.Expr -> Js.Ast
return_ =
  Js.AReturn . Just

builtinFunctions :: [Js.Ast]
builtinFunctions =
  [ builtinRightPipe
  ]

builtinRightPipe :: Js.Ast
builtinRightPipe =
  declFun "rightPipe" ["x", "f"] (call "f" ["x"])

call :: Text -> [Text] -> Js.Ast
call name = \case
  [] ->
    Js.AExpr $ Js.ECall (Js.EIdent name) []

  p:ps ->
    Js.AExpr $
      foldr
        (\x acc -> Js.ECall (Js.EIdent x) [acc] )
        (Js.ECall (Js.EIdent name) [Js.EIdent p])
        ps

declFun :: Text -> [Text] -> Js.Ast -> Js.Ast
declFun name args body =
  case reverse args of
    [] ->
      Js.AConst (mkBuiltinName name) (Js.ELambda [] body)

    a:as ->
      Js.AConst (mkBuiltinName name) $
        foldl'
          (\acc x -> Js.ELambda [x] (Js.AExpr acc))
          (Js.ELambda [a] body)
          as


toProgram :: [Expr] -> Js.Program
toProgram program =
  builtinFunctions ++ map exprToAst program

exprToAst :: Expr -> Js.Ast
exprToAst = \case
  ELet name e ->
    Js.AConst name (exprToExpr e)

  EBlock body ->
    let
      aux es = case es of
        [] ->
          [Js.AReturn Nothing]

        [e] -> case e of
          EUnit ->
            []

          ELet{} ->
            [exprToAst e]

          ERawJs js ->
            [Js.AExpr $ Js.ERawJs js]

          _ ->
            [return_ $ exprToExpr e]

        e:ess ->
          exprToAst e : aux ess
    in
    Js.ABlock $ aux body

  e ->
    Js.AExpr $ exprToExpr e

exprToExpr :: Expr -> Js.Expr
exprToExpr = \case
  EUnit ->
    Js.EIdent $ mkBuiltinName "unit"

  EInt n ->
    Js.EInt n

  EFloat f ->
    Js.EFloat f

  EString s ->
    Js.EString s

  EVar name ->
    Js.EIdent name

  EList xs ->
    Js.EList (map exprToExpr xs)

  EBinOp op l r ->
    case op of
      "|>" ->
        Js.ECall
          ( Js.ECall (Js.EIdent $ mkBuiltinName "rightPipe") [exprToExpr l]
          )
          [exprToExpr r]
      _ ->
        Js.EBinOp op (exprToExpr l) (exprToExpr r)

  EFun (Id name) body ->
    Js.ELambda [name] $ exprToAst body

  EFun Unit body ->
    Js.ELambda [] $ exprToAst body

  EApp l r ->
    Js.ECall (exprToExpr l) [exprToExpr r]

  ERawJs js ->
    Js.ERawJs js

  ELet{} ->
    error "expr let"

  EBlock{} ->
    error "expr block"

compile filepath src =
  Parser.run filepath src
    |> show +++ id
  >>= \asts ->
    Right $
      toProgram asts
      |> Js.gen
