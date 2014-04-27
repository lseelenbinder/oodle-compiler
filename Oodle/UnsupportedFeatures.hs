{-# LANGUAGE FlexibleInstances #-}

module Oodle.UnsupportedFeatures (unsupportedFeatures) where

import Oodle.Error (msgWithToken)
import Oodle.ParseTree (Start, Expression(..), isArrayType)
import Oodle.TreeWalker
import Oodle.SymbolTable (SymbolTable, buildType)

unsupportedFeatures :: SymbolTable -> Start -> [String]
unsupportedFeatures st = walk (st, (head st), (head st), False)

instance Walkable [String] where
  reduce = concatMap (filter (/= ""))

  doArgumentArr _ tk name _ _ = [msgWithToken tk "array indexing" name]

  doVariable _ tk name typ (expr, _)
    | expr /= ExpressionNoop  =
      [msgWithToken tk "variable with assignment" name]
    | isArrayType t           = [msgWithToken tk "array type variable" name]
    | otherwise               = []
    where t = buildType typ

  doAssignStmtArr _ tk name _ _ = [msgWithToken tk "array indexing" name]

  -- Nothing extra to do for these
  doClass _ _ _ _ _ _     = []
  doMethod _ _ _ _ _ _ _  = []
  doAssignStmt _ _ _ _    = []
  doIfStmt _ _ _ _ _      = []
  doLoopStmt _ _ _ _      = []
  doCallStmt _ _ _ _ _    = []
  doExpression _ _        = []
  doArgument _ _ _ _      = []
