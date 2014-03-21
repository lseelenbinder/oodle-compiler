{-# LANGUAGE FlexibleInstances #-}

module Oodle.SymbolTable
  (
    Symbol(..),
    symbolTableBuilder,
    SymbolTable,
    Declaration(..),
    getClassDecl,
    getMethodDecl,
    getNamedDecl,
    getType,
    getVariables,
    getParameterTypes,
    buildType,
    unbuildArray
    )
where

import Oodle.Error
import Oodle.ParseTree
import Oodle.Token
import Oodle.TreeWalker (Walkable(..))

type SymbolTable = [Symbol]

data Symbol = Symbol {
  symbol :: String,
  decl :: Declaration
  }
  deriving (Show, Eq)

data Declaration
    --                variables    methods
    = ClassDecl Token [Symbol]     [Symbol]
    --                parameters  parameter types   variables
    | MethodDecl Token Type Int         [Type]            [Symbol]
    | VarDecl    { varToken :: Token, type' :: Type }
    deriving (Show, Eq)

isVarDecl :: Symbol -> Bool
isVarDecl (Symbol _ VarDecl{}) = True
isVarDecl _ = False

isClassDecl :: Symbol -> Bool
isClassDecl (Symbol _ ClassDecl{}) = True
isClassDecl _ = False

isMethodDecl :: Symbol -> Bool
isMethodDecl (Symbol _ MethodDecl{}) = True
isMethodDecl _ = False

-- Build Symbol Table
symbolTableBuilder :: Start -> Error SymbolTable
symbolTableBuilder = walk

instance Walkable (Error SymbolTable) where

  walk (Start classes) = do
    base <- reduceMap walkClass classes
    return (getSymbolTable ++ base)

  reduce [] = return []
  reduce symboltables = do
    ss <- sequence symboltables --liftM concat (join symboltables)

    let joined = concat ss

    if null joined then return []
    else
      let
        reduce' tk kind symbols =
          if fst valid then return symbols'
          else
            fail $ msgWithToken' tk ("redeclared " ++ kind ++ " " ++ (\(Just s) ->  s) (snd valid))
          where symbols' = takeWhileFirst symbols
                valid = checkPush symbols'
      in
        case head joined of
          Symbol _ (VarDecl tk _) ->
            reduce' tk "variable" joined
          Symbol _ (MethodDecl tk _ _ _ _) ->
            reduce' tk "method" joined
          Symbol _ (ClassDecl tk _ _) ->
            reduce' tk "class" joined

  --doArgument _ (Id name) typ        = [pushVar name (buildType typ)]
  doArgument tk (IdArray name _) typ = Ok [pushVar tk name (buildType typ)]
  doArgument tk (Id name) typ = Ok [pushVar tk name (buildType typ)]
  doVariable tk (Id name) typ _ = Ok [pushVar tk name (buildType typ)]
  doMethod tk (Id name) typ args vars _ = do
    args' <- args
    vars' <- vars
    return [pushMethod tk name (buildType typ) (length args') args' vars']
  doClass tk (Id name) _ vars methods = do
    vars' <- vars
    methods' <- methods
    return [pushClass tk name vars' methods']

  doAssignStmt _ _ _  = Ok []
  doIfStmt _ _ _ _    = Ok []
  doLoopStmt _ _ _    = Ok []
  doCallStmt _ _ _ _  = Ok []
  doExpression _      = Ok []

checkPush :: [Symbol] -> (Bool, Maybe String)
checkPush symbols =
  let
    checkPush' :: [Symbol] -> [String] -> (Bool, Maybe String)
    checkPush' [] _ = (True, Nothing)
    checkPush' (s:xs) checked =
      if sym `elem` checked then
        (False, Just sym)
      else
        checkPush' xs (sym : checked)
      where sym = symbol s
  in
    checkPush' symbols []

getParameterTypes :: Declaration -> [Type]
getParameterTypes (MethodDecl _ _ _ types _) = types

getType :: Declaration -> Type
getType (MethodDecl _ t _ _ _) = t

getVariables :: Declaration -> [Symbol]
getVariables (ClassDecl _ vars _) = vars
getVariables (MethodDecl _ _ _ _ vars) = vars

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

-- Get a class declaration from a SymbolTable
getClassDecl :: SymbolTable -> String -> Error Declaration
getClassDecl = getNamedDecl

-- Get a method declaration from a parent class declaration
getMethodDecl :: Declaration -> String -> Error Declaration
getMethodDecl (ClassDecl _ _ methods) = getNamedDecl methods

getNamedDecl :: [Symbol] -> String -> Error Declaration
getNamedDecl symbols sym = do
  s <- findSymbol symbols sym
  return $ decl s

-- Symbol must exist
findSymbol :: [Symbol] -> String -> Error Symbol
findSymbol symbols sym =
  if not $ null match then return $ head match
  else fail $ "Undeclared variable/method: " ++ sym
  where match = dropWhile (\s -> symbol s /= sym) symbols

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

takeWhileFirst :: SymbolTable -> SymbolTable
takeWhileFirst st =
  case head st of
    Symbol _ VarDecl{} -> takeWhile isVarDecl st
    Symbol _ MethodDecl{} -> takeWhile isMethodDecl st
    Symbol _ ClassDecl{} -> takeWhile isClassDecl st

-- Handles building any kind of array type
buildType :: Type -> Type
buildType (TypeExp t _) = TypeArray (buildType t)
buildType t = t

unbuildArray :: Type -> Int -> Type
unbuildArray t 0 = t
unbuildArray (TypeArray t) i = unbuildArray t (i - 1)
