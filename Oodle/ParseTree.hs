module Oodle.ParseTree where

data Start
      = Start [Class] SymbolTable
  deriving Show

data Class
      -- [first] Id = Class Name
      -- [second] Id = Parent Name
      = Class Id Id [Var] [Method]
  deriving Show

data Var
      = Var Id Type Expression
  deriving Show

data Method
      -- Id = Method name
      -- Type = return type
      -- [Argument] = arguments
      -- [Var] = variable declarations
      -- [Statement] = statments
      = Method Id Type [Argument] [Var] [Statement]
  deriving Show

data Argument = Argument Id Type
  deriving Show

data Statement
      = AssignStatement Id Expression
      | IfStatement Expression [Statement] [Statement]
      | LoopStatement Expression [Statement]
      | CallStatement Expression Id [Expression]
  deriving Show

data Id
      = Id String
      | IdArray String [Expression]
  deriving (Show, Eq)

data Type
      = TypeInt
      | TypeNull
      | TypeString
      | TypeBoolean
      | TypeId Id
      | TypeExp Type Expression
  deriving (Show, Eq)

data Expression
      = ExpressionInt Int
      | ExpressionId Id
      | ExpressionStr String
      | ExpressionTrue
      | ExpressionFalse
      | ExpressionMe
      | ExpressionType Type
      | ExpressionCall Expression Id [Expression]
      | ExpressionIdArray Id
      | ExpressionNot Expression
      | ExpressionNeg Expression
      | ExpressionPos Expression
      | ExpressionMul Expression Expression
      | ExpressionDiv Expression Expression
      | ExpressionAdd Expression Expression
      | ExpressionSub Expression Expression
      | ExpressionStrCat Expression Expression
      | ExpressionEq Expression Expression
      | ExpressionGt Expression Expression
      | ExpressionGtEq Expression Expression
      | ExpressionAnd Expression Expression
      | ExpressionOr Expression Expression
      | ExpressionNull
  deriving (Show, Eq)

--                             Global   Class    Method
data SymbolTable = SymbolTable [Symbol] [Symbol] [Symbol]
  deriving (Show)

data Symbol
  = SymbolClass String Class
  | SymbolMethod String Method
  | SymbolVar String Var
  deriving (Show)

getSymbolTable :: SymbolTable
getSymbolTable = SymbolTable
  [
  -- in class (w/ readInt)
  SymbolClass "in" (Class (Id "Reader") (Id "") [] [Method (Id "readInt") TypeInt [] [] []]),
  -- out class (w/ writeInt)
  SymbolClass "out" (Class (Id "Writer") (Id "") [] [Method (Id "writeInt") TypeNull [Argument (Id "num") TypeInt] [] []])
  ] [] []

pushSymbolTable :: SymbolTable -> Symbol -> SymbolTable
pushSymbolTable (SymbolTable g c m) s = SymbolTable (g ++ [s]) c m

startClass :: SymbolTable -> String -> Class -> SymbolTable
startClass (SymbolTable g c m) symbol newClass = SymbolTable g (c ++ [SymbolClass symbol newClass]) m

verifySymbolTable :: SymbolTable -> String -> Bool
verifySymbolTable (SymbolTable g c m) symbol =
  vsl g && vsl c && vsl m
  where vsl = verifySymbolList symbol

verifySymbolList :: String -> [Symbol] -> Bool
verifySymbolList _ [] = True
verifySymbolList symbol (x:xs) = getSymbol x /= symbol && verifySymbolList symbol xs

getSymbol :: Symbol -> String
getSymbol (SymbolClass s _) = s
getSymbol (SymbolMethod s _) = s
getSymbol (SymbolVar s _) = s

mergeSymbolTable :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTable (SymbolTable g c m) (SymbolTable g' c' m') = SymbolTable (g ++ g') (c ++ c') (m ++ m')

pushClass :: SymbolTable -> SymbolTable
pushClass st = st

pushMethod :: SymbolTable -> SymbolTable
pushMethod st = st
