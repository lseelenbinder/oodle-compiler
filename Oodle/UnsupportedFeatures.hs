{-# LANGUAGE FlexibleInstances #-}

module Oodle.UnsupportedFeatures (unsupportedFeatures) where

import Oodle.Error (msgWithToken, msgWithToken')
import Oodle.ParseTree
import Oodle.TreeWalker
import Oodle.TypeChecker (buildType)

unsupportedFeatures :: Start -> [String]
unsupportedFeatures = walk

instance Walkable [String] where
  reduce = concatMap (filter (/= ""))

  doClass tk (Id name) (Id parent) _ _
    | parent /= ""            = [msgWithToken tk "class inheritance" name]
    | otherwise               = []

  doMethod tk (Id name) _ _ vars _
    | name /= "start"         = [msgWithToken tk "method other than 'start'" name]
    | not $ null vars         = [msgWithToken' tk "local variables in the start method"]
    | otherwise               = []

  doArgument tk (IdArray name exprs) typ
    | exprs /= []             = [msgWithToken tk "array indexing" name]
    | t == TypeString         = [msgWithToken tk "string type argument" name]
    | otherwise               = []
    where t = buildType typ

  doArgument tk (Id name) typ
    | t == TypeString         = [msgWithToken tk "string type argument" name]
    | otherwise               = []
    where t = buildType typ

  doVariable tk (Id name) typ expr
    | expr /= ExpressionNoop  = [msgWithToken tk "variable with assignment" name]
    | isArrayType t           = [msgWithToken tk "array type variable" name]
    | t == TypeString         = [msgWithToken tk "string type variable" name]
    | otherwise               = []
    where t = buildType typ

  doAssignStmt tk (IdArray name exprs) _
    | exprs /= []             = [msgWithToken tk "array indexing" name]
    | otherwise               = []

  -- Nothing extra to do for these
  doIfStmt   _ _ _ _  = []
  doLoopStmt _ _ _    = []
  doCallStmt _ _ _ _  = []

  doExpression expr =
    case expr of
      ExpressionNew   tk _  -> [msgWithToken' tk "new id expression"]
      ExpressionMe    tk    -> [msgWithToken' tk "me expression"]
      ExpressionNull  tk    -> [msgWithToken' tk "null expression"]
      ExpressionStr   tk _  -> [msgWithToken' tk "string literal"]
      _                     -> []
