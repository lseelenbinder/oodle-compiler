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
    -- global Reader & Writer
    Symbol "in" (VarDecl fakeToken (TypeId (Id "Reader"))),
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
  walk scope (Start classes) =
    let
      walk' :: Scope -> [Class] -> Error SymbolTable
      walk' (st, _, _, _) [] = return st
      walk' (st, cls, m, d) (c:clses) = do
        nc <- newClass
        walk' ((st ++ nc), cls, m, d) clses
        where newClass = walkClass (st, cls, m, d) c
    in
      walk' scope classes

  reduce [] = return []
  reduce symboltables = do
    ss <- sequence symboltables

    let joined = concat ss

    if null joined then return []
    else
      let
        reduce' tk kind symbols =
          case catchDuplicates (getSymbolTable ++ symbols') of
            Ok _      -> return $ symbols'
            Failed s  -> fail $ msgWithToken' tk
                                  ("redeclared " ++ kind ++ " " ++ s)
          where symbols' = takeWhileFirst symbols
      in
        case head joined of
          Symbol _ (VarDecl tk _) ->
            reduce' tk "variable" joined
          Symbol _ (MethodDecl tk _ _ _ _) ->
            reduce' tk "method" joined
          Symbol _ (ClassDecl tk _ _ _ _) ->
            reduce' tk "class" joined

  doVariable s tk name typ _    = doArgument s tk name typ
  doArgumentArr s tk name _ typ = doArgument s tk name typ
  doArgument _ tk name typ      = return [pushVar tk name (buildType typ)]

  doClass (st, _, _, _) tk name parentName (_, vars) (_, methods) = do
    parent <- if parentName /= "" then do
                case findSymbol st (parentName, tk) of
                  Ok p -> return p
                  Failed _ ->
                    fail $ msgWithToken tk "parent class does not exist" name
              else do
                if name == "ood" then
                  return $ pushClass fakeToken "" "" [] [] []
                else
                  findSymbol st ("ood", tk)
    vars' <- vars
    methods' <- methods
    let methods'' = (case name of
                      "Reader"  ->
                        [pushMethod fakeToken "io_read" TypeInt 0 [] []]
                      "Writer"  ->
                        [pushMethod fakeToken "io_write" typeNull 1
                          [pushVar fakeToken "" TypeInt] []]
                      _         -> []) ++ methods'
    return [pushClass tk name (symbol parent) (getVariables parent ++ vars')
              methods'' (buildInherited parent)]

    where buildInherited (Symbol name (ClassDecl _ _ _ methods inherited)) =
            inherited ++ (map (\m -> (name, m)) methods)

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
pushClass :: Token -> String -> String -> [Symbol] -> [Symbol] ->
              [(String, Symbol)] -> Symbol
pushClass tk sym parent vars methods inherited =
  Symbol sym (ClassDecl tk parent vars methods inherited)

-- Variable Declarations
pushVar :: Token -> String -> Type -> Symbol
pushVar tk sym t = Symbol sym (VarDecl tk t)

-- Method Declarations
pushMethod :: Token -> String -> Type -> Int -> [Symbol] -> [Symbol] -> Symbol
pushMethod tk sym t argCount args vars =
  Symbol sym (MethodDecl tk t argCount args' vars)
  where args' = map (type' . decl) args

catchDuplicates :: [Symbol] -> Error Bool
catchDuplicates symbols =
  let
    catchDuplicates' _ [] = return True
    catchDuplicates' checked (sym:xs) =
      if sym `elem` checked then
        fail $ show sym
      else
        catchDuplicates' (sym : checked) xs
  in
    catchDuplicates' [] symbols

takeWhileFirst :: SymbolTable -> SymbolTable
takeWhileFirst [] = []
takeWhileFirst st =
  case head st of
    Symbol _ VarDecl{}    -> takeWhile isVarDecl st
    Symbol _ MethodDecl{} -> takeWhile isMethodDecl st
    Symbol _ ClassDecl{}  -> takeWhile isClassDecl st
