{-# LANGUAGE FlexibleInstances #-}

module Oodle.UnsupportedFeatures (unsupportedFeatures) where

import Oodle.Error (msgWithToken, msgWithToken')
import Oodle.ParseTree
import Oodle.TreeWalker
import Oodle.SymbolTable (SymbolTable, buildType)

unsupportedFeatures :: SymbolTable -> Start -> [String]
unsupportedFeatures st = walk (st, (head st), (head st), False)

instance Walkable [String] where
  reduce = concatMap (filter (/= ""))

  doClass _ tk name parentName _ _
    | parentName /= ""        = [msgWithToken tk "class inheritance" name]
    | otherwise               = []

  doMethod _ tk name _ _ (vars, _) _
    | name /= "start" = [msgWithToken' tk "method other than 'start'"]
    | not $ null vars = [msgWithToken' tk "local variables in the start method"]
    | otherwise       = []

  doArgumentArr _ tk name _ typ =
      [msgWithToken tk "array indexing" name,
        if t == TypeString then
          msgWithToken tk "string type argument" name
        else ""
      ]
    where t = buildType typ

  doArgument _ tk name typ
    | t == TypeString         = [msgWithToken tk "string type argument" name]
    | otherwise               = []
    where t = buildType typ

  doVariable _ tk name typ (expr, _)
    | expr /= ExpressionNoop  =
      [msgWithToken tk "variable with assignment" name]
    | isArrayType t           = [msgWithToken tk "array type variable" name]
    | t == TypeString         = [msgWithToken tk "string type variable" name]
    | otherwise               = []
    where t = buildType typ

  doAssignStmt _ _ _ _ = []
  doAssignStmtArr _ tk name _ _ = [msgWithToken tk "array indexing" name]

  -- Nothing extra to do for these
  doIfStmt   _ _ _ _ _  = []
  doLoopStmt _ _ _ _    = []
  doCallStmt _ _ _ _ _  = []

  doExpression _ expr =
    case expr of
      ExpressionNew   tk _  -> [msgWithToken' tk "new id expression"]
      ExpressionMe    tk    -> [msgWithToken' tk "me expression"]
      ExpressionNull  tk    -> [msgWithToken' tk "null expression"]
      ExpressionStr   tk _  -> [msgWithToken' tk "string literal"]
      _                     -> []
