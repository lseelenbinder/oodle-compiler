{-# LANGUAGE FlexibleInstances #-}

module Oodle.CodeGenerator where

import Data.List (intercalate, findIndex, nub)
import Data.Maybe (fromMaybe)

import Oodle.SymbolTable (
  SymbolTable,
  Symbol(..),
  Declaration(..),
  findSymbol,
  findDecl,
  symbol,
  decl,
  isClassDecl,
  getMethods,
  getInheritedMethods,
  getVariables,
  resolveScope,
  Scope
  )
import Oodle.ParseTree
import Oodle.Error (Error(..), deE)
import Oodle.Token (Token(..), TokenPosition(..), printToken, fakeToken)
import Oodle.TypeChecker (typeOfExpression)

import Oodle.TreeWalker

codeGenerator :: SymbolTable -> Bool -> Start -> String
codeGenerator st debug start = buildString $ lines $ buildString [
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

  ".data",
  "\t.comm GLOBAL_OODLE_OBJECT_COUNT, 4, 4",
  "\t.comm in, 4, 4",
  "\t.comm out, 4, 4",
  ".text",

  ".global main",
  "main:",
  -- setup Main object
  push "$" ++ show ((length (getVariables entry) + 2) * 4),
  push "$1",
  "\tcall calloc",
  "\taddl $8, %esp",
  push "%eax",

  -- setup Reader object
  push "$" ++ show ((length (getVariables reader) + 2) * 4),
  push "$1",
  "\tcall calloc",
  "\taddl $8, %esp",
  "\tmovl %eax, (in)",

  -- setup Writer object
  push "$" ++ show ((length (getVariables writer) + 2) * 4),
  push "$1",
  "\tcall calloc",
  "\taddl $8, %esp",
  "\tmovl %eax, (out)",

  "\tcall " ++ (symbol entry) ++ "_start",
  push "$0",
  "\tcall exit",
  walk scope start,
  "\n"
  ]
  where entry = getEntryPoint st
        (ClassDecl tk _ _ _ _) = decl (st !! 4)
        filename = (getFilePath (getPosition tk))
        scope = (st, (head st), (head st), debug)
        reader = deE $ findSymbol st ("Reader", tk)
        writer = deE $ findSymbol st ("Writer", tk)

getEntryPoint :: SymbolTable -> Symbol
getEntryPoint st =
  let
    getEntryPoint' :: SymbolTable -> [Symbol]
    getEntryPoint' [] = []
    getEntryPoint' (c:cs) =
      if isClassDecl c then
        case findSymbol (getMethods c) ("start", fakeToken) of
          Failed _ -> getEntryPoint' cs
          Ok _ -> c : getEntryPoint' cs
      else
        getEntryPoint' cs
  in
    last (getEntryPoint' st)

instance Walkable String where
  reduce [] = []
  reduce strings =
    case nodeType of
      "#C"  -> buildString (nodeRest : tail strings)
      "#V"  -> nodeRest
      "#M"  -> nodeRest
      "#E"  -> nodeRest
      "#A"  -> nodeRest
      "#AS" -> nodeRest
      "#IS" -> nodeRest
      "#CS" -> nodeRest
      "#LS" -> nodeRest
      ""    -> buildString (nodeRest : tail strings)
      _     -> error $ show strings
    where (nodeType, nodeRest) = span (/= '\n') (head strings)

  doClass scope tk name parentName _ _ = buildString [
    "#C\n",
    (vft_label name) ++ ":",
    if parentName == "" && name /= "ood" then
      "\t.long " ++ vft_label "ood"
    else
      if name == "ood" then
        "\t.long 0"
      else
        "\t.long " ++ vft_label parentName
        ,

    buildString $ map (\(n, f) -> "\t.long " ++ n ++ "_" ++ f) vft
    ]
    where
          (_, c, _, _) = scope
          vft = buildVFT scope c

  doMethod scope _ name _ _ (vars, _) (_, stmts) = buildString [
    "#M",
    -- Debug
    if d then
      ".stabs  \"" ++ name' ++ ":f\",36,0,0," ++ name'
    else "",

    -- Method
    ".text",
    name' ++ ":",
    -- save and set %ebp
    push "%ebp",
    "\tmovl %esp, %ebp",

    -- reserve space for locals and return value (+1)
    buildString (take nLocals (repeat "\tpushl $0")),

    -- do the actual work
    stmts,

    -- save return value
    "\tmovl -4(%ebp), %eax",

    -- cleanup locals
    "\tmovl %ebp, %esp",

    pop "ebp",
    "\tret"
    ]
    where
      (_, _, _, d)  = scope
      name'         = varAssembly scope name
      nLocals       = length vars + 1

  doCallStmt scope tk (callScope', callScope) name (args, args') = buildString [
    "#CS",
    comment tk "Call",
    debugStatement scope tk,
    args', -- push arguments
    -- accounts for empty call scopes
    case callScope' of
      ExpressionNoop -> -- implict me
        push "8(%ebp)"
      _ -> callScope,

    -- check for null pointer
    nullPointerTest tk,

    "\tleal " ++ vft_label className ++ ", %eax",
    "\tcall *" ++ show (4 * (nthFunction + 1)) ++ "(%eax)",
    "\taddl $" ++ (show ((length args + 1) * 4)) ++ ", %esp" -- get rid of arguments & me (+1)
    ]
    where
      classDecl = deE $ resolveScope scope callScope'
      cls       = deE $ findDecl st (classDecl, tk)
      (st, _, _, _) = scope
      nthFunction   = getIndex symbol
                        (snd (unzip (getInheritedMethods cls)))
                        (length (getInheritedMethods cls) + getIndex symbol (getMethods cls) 0 name)
                        name
      className = symbol (cls)

  doVariable _ _ _ _ _ = "#V\n"

  doExpression scope expr =
    "#E\n" ++
    (case expr of
      ExpressionInt _ i               -> push ('$' : show i)
      ExpressionId _ (Id name)        -> buildString [setup, push offset]
        where (setup, offset) = calculateOffset scope name
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
      ExpressionEq tk expr1 expr2     -> doCompare tk "eq" expr1 expr2
      ExpressionGt tk expr1 expr2     -> doCompare tk "gt" expr1 expr2
      ExpressionGtEq tk expr1 expr2   -> doCompare tk "gteq" expr1 expr2

      -- Method Call
      ExpressionCall tk scopeExpr (Id name) args -> buildString [
        drop 4 $ doCallStmt (st, c, m, False) tk (scopeExpr, expr') name (args, args'),
        push "%eax\n" -- save return value (it's in eax)
        ]
        where (st, c, m, _) = scope
              expr' = walkExpression scope scopeExpr
              args' = reduceMap (walkExpression scope) args

      -- Objects
      ExpressionMe _                  -> push "8(%ebp)"
      ExpressionNew tk (TypeId (Id cls')) ->
        buildString [
          "\t# Allocate space for a new " ++ cls',

          -- calloc arguments
          -- # of object variables plus two reserved spots
          push "$" ++ show ((length vars + 2) * 4),
          push "$1",

          "\tcall calloc",
          "\tincl (GLOBAL_OODLE_OBJECT_COUNT)",

          -- cleanup after calloc
          "\taddl $8, %esp",

          -- save pointer to the VFT
          "\tmovl $" ++ vft_label cls' ++ ", (%eax)",

          -- push the pointer
          push "%eax"
          ]
        where (st, _, _, _) = scope
              (Ok cls) = findSymbol st (cls', tk)
              vars = getVariables cls
      ExpressionNull _                -> push "$0"

      -- Strings
      ExpressionStr tk string         -> buildString [
          ".data",
          label ++ ":",
          ".string \"" ++ string ++ "\"",
          ".text",
          push "$" ++ label,
          "\tcall string_fromlit", -- string_fromlit is in stdlib.c
          "\taddl $4, %esp",
          push "%eax"
        ]
        where label = buildLabelTag scope tk

      ExpressionStrCat tk expr1 expr2 -> doStringOp tk "cat" expr1 expr2

      {- THESE ARE UNSUPPORTED FEATURES
      ExpressionIdArray _ i           -> getVarType m cls i
      -}
      _                               -> "# TODO"
      )
      where doE = doExpression scope

            doCompare :: Token -> String -> Expression -> Expression -> String
            doCompare tk op expr1 expr2 =
              if t == typeString then
                doStringOp tk op expr1 expr2
              else
                cmp doE scope tk expr1 expr2 op
              where t = deE (typeOfExpression scope expr1)

            doStringOp :: Token -> String -> Expression -> Expression -> String
            doStringOp tk op str1 str2 = buildString [
                doE str2,
                -- check for null string pointer
                nullPointerTest tk,

                doE str1,
                -- check for null string pointer
                nullPointerTest tk,

                "\tcall String_" ++ op,
                "\taddl $8, %esp",
                push "%eax"
              ]

  doAssignStmt scope tk name (_, expr)  = buildString [
    "#AS",
    comment tk "Assignment",
    debugStatement scope tk,
    expr,
    pop "eax",
    setup,
    "\tmovl %eax, " ++ offset
    ]
    where (setup, offset) = calculateOffset scope name
  -- Arrays
  -- doAssignStmt (st, c, m) _ (IdArray name e) _  = error $ show name ++ show e

  doIfStmt scope tk (_, cond) (_, true) (_, false) = buildString [
    "#IS",
    comment tk "If",
    debugStatement scope tk,
    cond,
    pop "eax",
    "\tcmpl $1, %eax",
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
    "\tcmpl $1, %eax",
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

--                                     setup   offset
calculateOffset :: Scope -> String -> (String, String)
calculateOffset (st, c, m, d) name
  | name == "in" || name == "out" =
    ("", "(" ++ name ++ ")")
  -- Need to check if it is a local, a parameter, or a class var
  -- it is a local or a parameter
  | inM =
    ("", calculateLocalOffset (st, c, m, d) name)
  -- it is a class variable
  | otherwise = (
    -- evaluate local class
    "\tmovl 8(%ebp), %ebx",
    -- offset
    show ((nthCVar + 2) * 4 )++ "(%ebx) # offset of " ++ name
    )
  where inM     = case findSymbol (getVariables m) (name, fakeToken) of
                          Ok _ -> True
                          Failed _ -> False
        nthCVar = getIndex symbol (getVariables c) 0 name

calculateLocalOffset :: Scope -> String -> String
calculateLocalOffset (_, _, m, _) name
  | nthVar == nthM  = -- return value
    "-4(%ebp) # offset of return value for (" ++ name ++ ")"
  | nthVar > nthM   = -- it is a local
    "-" ++ (show ((nthVar - nthM + 1) * 4)) ++ "(%ebp) # offset of " ++ name
  | otherwise       = -- it is a parameter
    -- +3 because of saved %ebp, return address, and class me
    show (((nParams - nthVar - 1) + 3) * 4) ++ "(%ebp) # offset of " ++ name
  where nthVar  = getIndex symbol (getVariables m) 0 name
        nthM    = getIndex symbol (getVariables m) 0 (symbol m)
        (MethodDecl _ _ nParams _ _) = decl m

varAssembly :: Scope -> String -> String
varAssembly (_, c, _, _) var = (symbol c) ++ "_" ++ var

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
    "\tcmpl %ebx, %eax",
    "\tj" ++ (case op of
                "eq" -> "e"
                "gt" -> "g"
                "gteq" -> "ge"
                _     -> error "Unsupported comparison"
                )
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

nullPointerTest :: Token -> String
nullPointerTest tk = buildString [
    push "$" ++ show (getLineNo (getPosition tk)),
    "\tcall nullpointertest",
    "\taddl $4, %esp"
  ]


debugStatement :: Scope -> Token -> String
debugStatement (st, c, m, d) tk =
  if d then
    ".stabn 68,0," ++ lno ++ "," ++ label ++ "\n" ++ ".line" ++ lno ++ ":"
  else ""
  where lno = show (getLineNo (getPosition tk))
        name = varAssembly (st, c, m, d) (symbol m)
        label = ".line" ++ lno ++ "-" ++ name

buildLabelTag :: Scope -> Token -> String
buildLabelTag (_, _, m, _) tk =
  symbol m ++ "_" ++ show (getLineNo (getPosition tk)) ++ show (getCol (getPosition tk))

getIndex :: (Eq a) => (b -> a) -> [b] -> Int -> a -> Int
getIndex f haystack err needle =
  fromMaybe (err) (findIndex (\s -> needle == f s) haystack)

vft_label :: String -> String
vft_label = (++) "VFT"

buildVFT :: Scope -> Symbol -> [(String, String)]
buildVFT (st, _, _, _) (Symbol name (ClassDecl tk "" vars methods _)) =
  map (\m -> (name, symbol m)) (methods)
buildVFT scope (Symbol name (ClassDecl tk parentName vars methods _)) =
  nub (overwritten ++ new)
  where (st, _, _, _) = scope
        parent = deE $ findSymbol st (parentName, tk)
        parentVTF = buildVFT scope parent

        overwritten = map (\(c, f) -> case findSymbol methods (f, fakeToken) of
                                        Ok _ -> (name, f)
                                        Failed _ -> (c, f)) parentVTF
        new         = map (\m -> (name, symbol m)) (methods)
