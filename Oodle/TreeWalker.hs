{-# LANGUAGE FlexibleInstances #-}

module Oodle.TreeWalker (Walkable(..)) where

import Oodle.ParseTree
import Oodle.Token (Token)

class Walkable a where
  walk            :: (Walkable a) => Start -> a
  walkClass       :: (Walkable a) => Class -> a
  walkMethod      :: (Walkable a) => Method -> a
  walkArgument    :: (Walkable a) => Argument -> a
  walkVariable    :: (Walkable a) => Var -> a
  walkStatement   :: (Walkable a) => Statement -> a
  walkExpression  :: (Walkable a) => Expression -> a

  -- doClass token name parentName variables methods
  doClass         :: (Walkable a) => Token -> Id -> Id -> a -> a -> a

  -- doMethod token name type arguments variables statments
  doMethod        :: (Walkable a) => Token -> Id -> Type -> a -> a -> a -> a

  -- doArgument token (IdArray name expressions) type
  doArgument      :: (Walkable a) => Token -> Id -> Type -> a

  -- doVariable token name type expression
  doVariable      :: (Walkable a) => Token -> Id -> Type -> Expression -> a

  -- doAssignStmt token (IdArray varName) expression
  doAssignStmt    :: (Walkable a) => Token -> Id -> Expression -> a

  -- doIfStmt token condition trueStatments falseStatments
  doIfStmt        :: (Walkable a) =>
                      Token -> Expression -> [Statement] -> [Statement] -> a

  -- doLoopStmt token condition statments
  doLoopStmt      :: (Walkable a) => Token -> Expression -> [Statement] -> a

  -- doCallStmt token scope methodName arguments
  doCallStmt      :: (Walkable a) => Token -> Expression -> Id -> [Expression] -> a
  -- doExpression  expression
  doExpression    :: (Walkable a) => Expression -> a

  reduce          :: (Walkable a) => [a] -> a
  reduceMap       :: (Walkable a) => (b -> a) -> [b] -> a
  reduceMap f list = reduce $ map f list

  walk (Start classes) =
    reduceMap walkClass classes

  walkClass (Class tk className parentName vars methods) =
    reduce [doClass tk className parentName v m, v, m]
    where v = reduceMap walkVariable vars
          m = reduceMap walkMethod methods

  walkMethod (Method tk name typ arguments vars stmts) =
      reduce [doMethod tk name typ a v s, a, v, s]
    where a = reduceMap walkArgument arguments
          v = reduceMap walkVariable vars
          s = reduceMap walkStatement stmts

  walkArgument (Argument tk (IdArray name exprs) typ) =
    reduce (doArgument tk (IdArray name exprs) typ : e)
    where e = map walkExpression exprs

  walkArgument (Argument tk (Id name) typ) =
    doArgument tk (Id name) typ

  walkVariable (Var tk name t expr) =
    reduce [doVariable tk name t expr, e]
    where e = walkExpression expr

  walkStatement s =
    let
      walkSParts expr stmts =
        reduce (e : ds)
        where e  = walkExpression expr
              ds = map walkStatement stmts
    in
      case s of
        AssignStatement tk (IdArray name exprs) expr ->
          reduce [doAssignStmt tk (IdArray name exprs) expr, e, e']
          where e  = walkExpression expr
                e' = reduceMap walkExpression exprs
        IfStatement tk expr trueStmts falseStmts ->
          reduce [doIfStmt tk expr trueStmts falseStmts, children]
          where children = walkSParts expr (trueStmts ++ falseStmts)
        LoopStatement tk expr stmts ->
          reduce [doLoopStmt tk expr stmts, children]
          where children = walkSParts expr stmts
        CallStatement tk expr name arguments ->
          reduce [doCallStmt tk expr name arguments, e, a]
          where e = walkExpression expr
                a = reduceMap walkExpression arguments

  walkExpression = doExpression
