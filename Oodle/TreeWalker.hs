module Oodle.TreeWalker (Walkable(..)) where

import Oodle.SymbolTable (
  findSymbol,
  getMethods,
  Scope
  )
import Oodle.ParseTree
import Oodle.Token
import Oodle.Error

class Walkable a where

  walk            :: (Walkable a) => Scope -> Start -> a
  walkClass       :: (Walkable a) => Scope -> Class -> a
  walkMethod      :: (Walkable a) => Scope -> Method -> a
  walkArgument    :: (Walkable a) => Scope -> Argument -> a
  walkVariable    :: (Walkable a) => Scope -> Var -> a
  walkStatement   :: (Walkable a) => Scope -> Statement -> a
  walkExpression  :: (Walkable a) => Scope -> Expression -> a

  -- doClass token name parentName variables methods
  doClass         :: (Walkable a) =>
                        Scope -> Token -> String -> String -> ([Var], a) ->
                        ([Method], a) -> a

  -- doMethod token name type arguments variables statments
  doMethod        :: (Walkable a) =>
                        Scope -> Token -> String -> Type -> ([Argument], a) ->
                        ([Var], a) -> ([Statement], a) -> a

  -- doArgument token name type
  doArgument      :: (Walkable a) => Scope -> Token -> String -> Type -> a
  -- doArgumentArr token (IdArray name expressions) type
  doArgumentArr   :: (Walkable a) =>
                        Scope -> Token -> String -> ([Expression], a) -> Type
                        -> a

  -- doVariable token name type expression
  doVariable      :: (Walkable a) =>
                        Scope -> Token -> String -> Type -> (Expression, a) -> a

  -- doAssignStmtArr token varName arrayScope expression
  doAssignStmtArr :: (Walkable a) =>
                        Scope -> Token -> String -> ([Expression], a) ->
                        (Expression, a) -> a
  -- doAssignStmt token varName expression
  doAssignStmt    :: (Walkable a) =>
                        Scope -> Token -> String -> (Expression, a) -> a

  -- doIfStmt token conditional trueStatments falseStatments
  doIfStmt        :: (Walkable a) =>
                        Scope -> Token -> (Expression, a) -> ([Statement], a)
                        -> ([Statement], a) -> a

  -- doLoopStmt token conditional statments
  doLoopStmt      :: (Walkable a) =>
                        Scope -> Token -> (Expression, a) -> ([Statement], a)
                        -> a

  -- doCallStmt token callScope methodName arguments
  doCallStmt      :: (Walkable a) =>
                        Scope -> Token -> (Expression, a) -> String ->
                        ([Expression], a) -> a
  -- doExpression  expression
  doExpression    :: (Walkable a) => Scope -> Expression -> a

  reduce          :: (Walkable a) => [a] -> a
  reduceMap       :: (Walkable a) => (b -> a) -> [b] -> a
  reduceMap f list = reduce $ map f list

  walk scope (Start classes) =
    reduceMap (walkClass scope) classes

  walkClass (st, _, _, debug) (Class tk (Id className) (Id parentName) vars methods) =
      reduce [doClass nScope tk className parentName (vars, v) (methods, m), v, m]
    where (Ok c)  = findSymbol st (className, tk)
          nScope  = (st, c, c, debug)
          v       = reduceMap (walkVariable nScope) vars
          m       = reduceMap (walkMethod nScope) methods

  walkMethod (st, c, _, debug) (Method tk (Id name) typ args vars stmts) =
      reduce [doMethod nScope tk name typ (args, a) (vars, v) (stmts, s), a, v, s]
    where (Ok m)  = findSymbol (getMethods c) (name, tk)
          nScope  = (st, c, m, debug)
          a       = reduceMap (walkArgument nScope) args
          v       = reduceMap (walkVariable nScope) vars
          s       = reduceMap (walkStatement nScope) stmts

  walkArgument scope (Argument tk (IdArray name exprs) typ) =
      reduce [doArgumentArr scope tk name (exprs, e) typ, e]
    where e = reduceMap (walkExpression scope) exprs

  walkArgument scope (Argument tk (Id name) typ) =
    doArgument scope tk name typ

  walkVariable scope (Var tk (Id name) t expr) =
      reduce [doVariable scope tk name t (expr, e), e]
    where e = walkExpression scope expr

  walkStatement scope s =
    let
      walkStmts  = reduceMap (walkStatement scope)
      walkExpr    = walkExpression scope
    in
      case s of
        AssignStatement tk (IdArray varName arrayScope) expr ->
            reduce [doAssignStmtArr scope tk varName (arrayScope, arrayScope')
                    (expr, e), e, arrayScope']
          where e           = walkExpr expr
                arrayScope' = reduceMap walkExpr arrayScope

        AssignStatement tk (Id varName) expr ->
            reduce [doAssignStmt scope tk varName (expr, e), e]
          where e           = walkExpr expr

        IfStatement tk cond trueStmts falseStmts ->
            reduce [doIfStmt scope tk (cond, cond') (trueStmts, trueStmts')
                    (falseStmts, falseStmts'),
                    cond', trueStmts', falseStmts']
          where cond'       = walkExpr cond
                trueStmts'  = walkStmts trueStmts
                falseStmts' = walkStmts falseStmts

        LoopStatement tk cond children ->
            reduce [doLoopStmt scope tk (cond, cond') (children, children'),
                    cond', children']
          where cond'     = walkExpr cond
                children' = walkStmts children

        CallStatement tk callScope (Id name) arguments ->
            reduce [doCallStmt scope tk (callScope, callScope') name
                    (arguments, arguments'), callScope', arguments']
          where callScope'  = walkExpr callScope
                arguments'  = reduceMap walkExpr arguments

  walkExpression scope expr = reduce [doExpression scope expr]
