{-# LANGUAGE FlexibleInstances #-}

module Oodle.CodeGenerator where

import Data.List (intercalate)

import Oodle.SymbolTable (
  SymbolTable,
  Symbol,
  Declaration(..),
  findSymbol,
  symbol,
  decl,
  isClassDecl,
  getMethods,
  getType,
  getMethodDecl,
  resolveScope,
  Scope
  )
import Oodle.ParseTree
import Oodle.Error (Error(..), deE)
import Oodle.Token (Token(..), TokenPosition(..), printToken)

import Oodle.TreeWalker

codeGenerator :: SymbolTable -> Bool -> Start -> String
codeGenerator st debug start = buildString [
  if debug then
    concat [
      ".file   \"" ++ filename ++ "\"\n",
      "\t.stabs \"" ++ filename ++ "\",100,0,0,.ltext0\n",
      "\t.text\n",
      ".ltext0:\n",
      "\t.stabs  \"int:t(0,1)=r(0,1);-2147483648;2147483647;\",128,0,0,0\n"
    ]
  else "",
  "STDOUT = 1",
  "STDIN = 0",
  "",
  ".text",
  ".global main",
  "main:",
  "\tcall " ++ entry ++ "_start",
  "\tpushl $0",
  "\tcall exit",
  walk scope start,
  "\n"
  ]
  where entry = getEntryPoint st
        (ClassDecl tk _ _) = decl (st !! 4)
        filename = (getFilePath (getPosition tk))
        scope = (st, (head st), (head st), debug)

varAssembly :: Symbol -> Symbol -> String -> String
varAssembly c m var =
  prefix ++ "_" ++ var

  where m' = findSymbol (getMethods c) var
        isM = case m' of
                Ok _ -> True
                _ -> False
        prefix = symbol (if isM then m else c)

getEntryPoint :: SymbolTable -> String
getEntryPoint st =
  let
    getEntryPoint' :: SymbolTable -> [String]
    getEntryPoint' [] = []
    getEntryPoint' (c:cs) =
      if isClassDecl c then
        case findSymbol (getMethods c) "start" of
          Failed _ -> getEntryPoint' cs
          Ok _ -> (symbol c) : getEntryPoint' cs
      else
        getEntryPoint' cs
  in
    last (getEntryPoint' st)

instance Walkable String where
  reduce [] = []
  reduce strings =
    case nodeType of
      "#C"  -> buildString (nodeRest : tail strings)
      "#M"  -> nodeRest
      "#E"  -> nodeRest
      "#A"  -> nodeRest
      "#V"  -> buildString (nodeRest : tail strings)
      "#AS" -> nodeRest
      "#IS" -> nodeRest
      "#CS" -> nodeRest
      "#LS" -> nodeRest
      _     -> buildString (nodeRest : tail strings)
    where (nodeType, nodeRest) = span (/= '\n') (head strings)

  doClass _ _ _ _ _ _ = "#C\n"

  doMethod (_, c, _, d) _ name _ _ (_, vars) (_, stmts) = buildString [
    "#M",
    if d then
      ".stabs  \"" ++ name' ++ ":f\",36,0,0," ++ name'
    else "",
    ".text",
    name' ++ ":",
    vars,
    ".text",
    stmts,
    "\tret"
    ]
    where  name' = varAssembly c c name

  doVariable (_, c, m, d) _ name _ _ = buildString [
    "#V",
    ".data",
    if d then ".stabs  \"" ++ var ++ ":g(0,1)\",32,0,0,0" else "",
    "\t.comm " ++ var ++ ", 4, 4"
    ]
    where var = varAssembly c m name

  doExpression scope expr =
    let
      doE = doExpression scope
    in
      "#E\n" ++
      (case expr of
        ExpressionInt _ i               -> push ('$' : show i)
        ExpressionId _ (Id name)        ->
          if name == "out" || name == "in" then
            ""
          else
            push "(" ++ (varAssembly c m name) ++ ")"
          where (_, c, m, _) = scope
        ExpressionTrue _                -> push "$1"
        ExpressionFalse _               -> push "$0"
        ExpressionNoop                  -> ""
        ExpressionNeg _ expr1           -> buildString [doE expr1,
                                            pop "eax",
                                            "\tnegl %eax",
                                            push "%eax"
                                          ]
        ExpressionPos _ _               -> "" -- essentially a no-op

        -- Basic Arithmetic
        ExpressionMul _ expr1 expr2     -> arithmetic doE expr1 expr2 "imull"
        ExpressionDiv _ expr1 expr2     -> arithmetic doE expr1 expr2 "idivl"
        ExpressionAdd _ expr1 expr2     -> arithmetic doE expr1 expr2 "addl"
        ExpressionSub _ expr1 expr2     -> arithmetic doE expr1 expr2 "subl"

        -- Boolean Operators
        ExpressionAnd _ expr1 expr2     -> arithmetic doE expr1 expr2 "andl"
        ExpressionOr _ expr1 expr2      -> arithmetic doE expr1 expr2 "orl"
        ExpressionNot _ expr1           -> buildString [
                                            doE expr1,
                                            pop "eax",
                                            "\txorl $1, %eax",
                                            push "%eax"
                                          ]

        -- Comparison Operators
        ExpressionEq tk expr1 expr2     -> cmp doE scope tk expr1 expr2 "eq"
        ExpressionGt tk expr1 expr2     -> cmp doE scope tk expr1 expr2 "gt"
        ExpressionGtEq tk expr1 expr2   -> cmp doE scope tk expr1 expr2 "gteq"

        ExpressionCall tk scopeExpr (Id name) args ->
          drop 4 $ doCallStmt (st, c, m, False) tk (scopeExpr, expr') name (args, args')
          where (st, c, m, _) = scope
                expr' = walkExpression scope scopeExpr
                args' = reduceMap (walkExpression scope) args

        _                               -> "# TODO"

        {-
        THESE ARE UNSUPPORTED FEATURES

        ExpressionStr _ _               -> return TypeString
        ExpressionMe _                  -> return $ TypeId (Id "me")
        ExpressionNew _ t               -> return t
        ExpressionNull _                -> return TypeNull

        ExpressionIdArray _ i           -> getVarType m cls i
        ExpressionStrCat _ expr1 expr2  -> checkBothTypeE' expr1 expr2 TypeString
        -}
        )

  doAssignStmt scope tk name (_, expr)  = buildString [
    "#AS",
    comment tk "Assignment",
    debugStatement scope tk,
    expr,
    pop "eax",
    "\tmovl %eax, (" ++ var ++ ")"
    ]
    where (_, c, m, _) = scope
          var = varAssembly c m name
  -- Arrays
  -- doAssignStmt (st, c, m) _ (IdArray name e) _  = error $ show name ++ show e

  doCallStmt scope tk (callScope', callScope) name (args, args') = buildString [
    "#CS",
    comment tk "Call",
    debugStatement scope tk,
    callScope,
    args',
    "\tcall " ++ name,
    buildString (take (length args) (repeat (pop "ebx"))), -- get rid of arguments
    (if getType method /= TypeNull then
      push "%eax" -- save return value (it's in eax)
    else "")
    ]
    where method = deE $ getMethodDecl (deE $ resolveScope scope callScope') name

  doIfStmt scope tk (_, cond) (_, true) (_, false) = buildString [
    "#IS",
    comment tk "If",
    debugStatement scope tk,
    cond,
    pop "eax",
    "\tcmp $1, %eax",
    "\tjnz startFalse" ++ tag,
    true,
    "\tjmp endFalse" ++ tag,
    "startFalse" ++ tag ++ ":",
    false,
    "endFalse" ++ tag ++ ":"
    ]
    where tag = buildLabelTag scope tk

  doLoopStmt scope tk (_, cond) (_, children) = buildString [
    "#LS",
    comment tk "Loop",
    debugStatement scope tk,
    start,
    cond,
    pop "eax",
    "\tcmp $1, %eax",
    "\tjnz " ++ (init done),
    children,
    "\tjmp " ++ (init start),
    done
    ]
    where
          tag   = buildLabelTag scope tk
          start = "startLoop" ++ tag ++ ":"
          done  = "doneLoop" ++ tag ++ ":"

  doArgument _ _ _ _    = "#A\n"

buildString :: [String] -> String
buildString = intercalate "\n" . filter (/= "")

arithmetic :: (Expression -> String) -> Expression -> Expression -> String -> String
arithmetic f expr1 expr2 op =
  buildString [
    f expr1,
    f expr2,
    pop "ebx", -- Expression 2
    pop "eax", -- Expression 1
    doMDorAS op,
    push "%eax"
    ]

doMDorAS :: String -> String
doMDorAS op
  | op == "imull" || op == "idivl" = concat [
    "\tmovl %eax, %edx\n",
    "\tsarl $31, %edx\n",
    "\t" ++ op ++ " %ebx\n" -- eax = e1 op e2
    ]
  | otherwise = "\t" ++ op ++ " %ebx, %eax" -- eax = e1 op e2

cmp :: (Expression -> String) -> Scope -> Token -> Expression -> Expression -> String -> String
cmp f scope tk expr1 expr2 op =
  buildString [
    f expr1,
    f expr2,
    pop "ebx", -- Expression 2
    pop "eax", -- Expression 1
    "\tcmp %ebx, %eax",
    "\tj" ++ (case op of
                "eq" -> "e"
                "gt" -> "g"
                "gteq" -> "ge")
          ++ " " ++ (init noDone),
    push "$0",
    "\tjmp " ++ (init yesDone),
    noDone,
    push "$1",
    yesDone
  ]
  where tag     = buildLabelTag scope tk
        yesDone = "yes" ++ tag ++ ":"
        noDone  = "no" ++ tag ++ ":"

push :: String -> String
push = (++) "\tpushl "

pop :: String -> String
pop = (++) "\tpopl %"

comment :: Token -> String -> String
comment tk typ = "# {{ " ++ typ ++ " }} " ++ printToken tk

debugStatement :: Scope -> Token -> String
debugStatement (_, c, m, d) tk =
  if d then
    ".stabn 68,0," ++ lno ++ "," ++ label ++ "\n" ++ ".line" ++ lno ++ ":"
  else ""
  where lno = show (getLineNo (getPosition tk))
        name = varAssembly c m (symbol m)
        label = ".line" ++ lno ++ "-" ++ name

buildLabelTag :: Scope -> Token -> String
buildLabelTag (_, _, m, _) tk =
  symbol m ++ "_" ++ show (getLineNo (getPosition tk))
