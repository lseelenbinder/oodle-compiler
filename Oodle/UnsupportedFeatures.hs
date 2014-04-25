{-# LANGUAGE FlexibleInstances #-}

module Oodle.UnsupportedFeatures (unsupportedFeatures) where

import Oodle.Error (msgWithToken)
import Oodle.ParseTree
import Oodle.TreeWalker
import Oodle.SymbolTable (SymbolTable, buildType)

unsupportedFeatures :: SymbolTable -> Start -> [String]
unsupportedFeatures st = walk (st, (head st), (head st), False)

instance Walkable [String] where
  reduce = concatMap (filter (/= ""))

  doClass _ _ _ _ _ _ = []

  doMethod _ _ _ _ _ _ _ = []

  doArgumentArr _ tk name _ _ =
      [msgWithToken tk "array indexing" name]

  doArgument _ _ _ _ = []

  doVariable _ tk name typ (expr, _)
    | expr /= ExpressionNoop  =
      [msgWithToken tk "variable with assignment" name]
    | isArrayType t           = [msgWithToken tk "array type variable" name]
    | otherwise               = []
    where t = buildType typ

  doAssignStmt _ _ _ _ = []
  doAssignStmtArr _ tk name _ _ = [msgWithToken tk "array indexing" name]

  -- Nothing extra to do for these
  doIfStmt   _ _ _ _ _  = []
  doLoopStmt _ _ _ _    = []
  doCallStmt _ _ _ _ _  = []
  doExpression _ _ = []
