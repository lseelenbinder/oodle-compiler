{-# LANGUAGE FlexibleInstances #-}
module Oodle.SymbolTableBuilder (symbolTableBuilder, getSymbolTable) where

import Oodle.Error
import Oodle.Token
import Oodle.ParseTree
import Oodle.SymbolTable
import Oodle.TreeWalker

getSymbolTable :: SymbolTable
getSymbolTable =
  [
  -- global Reader
  Symbol "Reader" (ClassDecl fakeToken [] [Symbol "readint" (MethodDecl fakeToken TypeInt 0 [] [])]),
  Symbol "in" (VarDecl fakeToken (TypeId (Id "Reader"))),
  -- global Writer
  Symbol "Writer" (ClassDecl fakeToken [] [Symbol "writeint" (MethodDecl fakeToken TypeNull 1 [TypeInt] [])]),
  Symbol "out" (VarDecl fakeToken (TypeId (Id "Writer")))
  ]

-- Build Symbol Table
symbolTableBuilder :: Start -> Error SymbolTable
symbolTableBuilder start = do
    st <- walk scope start
    return $ getSymbolTable ++ st
  where base = getSymbolTable
        scope = ([], (head base), (head base), False)

instance Walkable (Error SymbolTable) where

  reduce [] = return []
  reduce symboltables = do
    ss <- sequence symboltables

    let joined = concat ss

    if null joined then return []
    else
      let
        reduce' tk kind symbols =
          case catchDuplicates (getSymbolTable ++ symbols') [] of
            Ok _      -> return $ symbols'
            Failed s  -> fail $ msgWithToken' tk ("redeclared " ++ kind ++ " " ++ s)
          where symbols' = takeWhileFirst symbols
      in
        case head joined of
          Symbol _ (VarDecl tk _) ->
            reduce' tk "variable" joined
          Symbol _ (MethodDecl tk _ _ _ _) ->
            reduce' tk "method" joined
          Symbol _ (ClassDecl tk _ _) ->
            reduce' tk "class" joined

  doVariable s tk name typ _    = doArgument s tk name typ
  doArgumentArr s tk name _ typ = doArgument s tk name typ
  doArgument _ tk name typ      = return [pushVar tk name (buildType typ)]

  doClass _ tk name _ (_, vars) (_, methods) = do
    vars' <- vars
    methods' <- methods
    return [pushClass tk name vars' methods']

  doMethod _ tk name typ (_, args) (_, vars) _ = do
    args' <- args
    vars' <- vars
    return [pushMethod tk name (buildType typ) (length args') args'
              (concat [args', [pushVar tk name typ], vars'])]

  doAssignStmt _ _ _ _      = return []
  doAssignStmtArr _ _ _ _ _ = return []
  doIfStmt _ _ _ _ _        = return []
  doLoopStmt _ _ _ _        = return []
  doCallStmt _ _ _ _ _      = return []
  doExpression _ _          = return []

-- Class Declarations
pushClass :: Token -> String -> [Symbol] -> [Symbol] -> Symbol
pushClass tk sym var methods = Symbol sym (ClassDecl tk var methods)

-- Variable Declarations
pushVar :: Token -> String -> Type -> Symbol
pushVar tk sym t = Symbol sym (VarDecl tk t)

-- Method Declarations
pushMethod :: Token -> String -> Type -> Int -> [Symbol] -> [Symbol] -> Symbol
pushMethod tk sym t argCount args vars =
  Symbol sym (MethodDecl tk t argCount args' vars)
  where args' = map (type' . decl)  args

catchDuplicates :: [Symbol] -> [String] -> Error Bool
catchDuplicates [] _ = return True
catchDuplicates (s:xs) checked =
  if sym `elem` checked then
    fail $ show sym
  else
    catchDuplicates xs (sym : checked)
  where sym = symbol s

takeWhileFirst :: SymbolTable -> SymbolTable
takeWhileFirst [] = []
takeWhileFirst st =
  case head st of
    Symbol _ VarDecl{}    -> takeWhile isVarDecl st
    Symbol _ MethodDecl{} -> takeWhile isMethodDecl st
    Symbol _ ClassDecl{}  -> takeWhile isClassDecl st
