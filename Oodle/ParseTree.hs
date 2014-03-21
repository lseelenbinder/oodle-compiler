module Oodle.ParseTree where

import Oodle.Token (Token, fakeToken)

data Start
      = Start [Class]
  deriving (Show, Eq)

data Class
      -- [first] Id = Class Name
      -- [second] Id = Parent Name
      = Class Token Id Id [Var] [Method]
  deriving (Show, Eq)

data Var
      = Var Token Id Type Expression
  deriving (Show, Eq)

data Method
      -- Id = Method name
      -- Type = return type
      -- [Argument] = arguments
      -- [Var] = variable declarations
      -- [Statement] = statments
      = Method Token Id Type [Argument] [Var] [Statement]
  deriving (Show, Eq)

data Argument = Argument Token Id {- Id is an IdArray -} Type
  deriving (Show, Eq)

data Statement
      = AssignStatement Token Id {- Id is an IdArray -} Expression
      --                    conditional if stmts    else stmts
      | IfStatement   Token Expression [Statement] [Statement]
      --                    conditional do stmts
      | LoopStatement Token Expression [Statement]
      --                    scope     name arguments
      | CallStatement Token Expression Id [Expression]
  deriving (Show, Eq)

data Id
      = Id { getIdString :: String}
      | IdArray String [Expression]
  deriving (Show, Eq)

data Type
      = TypeInt
      | TypeNull
      | TypeString
      | TypeBoolean
      | TypeId {getId :: Id }
      | TypeExp Type Expression
      | TypeArray Type
      | TypeNoop
  deriving (Show, Eq)

isArrayType :: Type -> Bool
isArrayType (TypeArray _) = True
isArrayType _ = False

data Expression
      = ExpressionInt Token Int
      | ExpressionId Token Id
      | ExpressionStr Token String
      | ExpressionTrue Token
      | ExpressionFalse Token
      | ExpressionMe Token
      | ExpressionNew Token Type
      | ExpressionCall Token Expression Id [Expression]
      | ExpressionIdArray Token Id
      | ExpressionNot Token Expression
      | ExpressionNeg Token Expression
      | ExpressionPos Token Expression
      | ExpressionMul Token Expression Expression
      | ExpressionDiv Token Expression Expression
      | ExpressionAdd Token Expression Expression
      | ExpressionSub Token Expression Expression
      | ExpressionStrCat Token Expression Expression
      | ExpressionEq Token Expression Expression
      | ExpressionGt Token Expression Expression
      | ExpressionGtEq Token Expression Expression
      | ExpressionAnd Token Expression Expression
      | ExpressionOr Token Expression Expression
      | ExpressionNull Token
      | ExpressionNoop -- noop
  deriving (Show, Eq)

getExprToken :: Expression -> Token
getExprToken expr =
  case expr of
      ExpressionInt tk _    -> tk
      ExpressionId tk _     -> tk
      ExpressionStr tk _    -> tk
      ExpressionTrue tk     -> tk
      ExpressionFalse tk    -> tk
      ExpressionMe tk       -> tk
      ExpressionNew tk _    -> tk
      ExpressionCall tk _ _ _ -> tk
      ExpressionIdArray tk _  -> tk
      ExpressionNot tk _    -> tk
      ExpressionNeg tk _    -> tk
      ExpressionPos tk _    -> tk
      ExpressionMul tk _ _  -> tk
      ExpressionDiv tk _ _  -> tk
      ExpressionAdd tk _ _  -> tk
      ExpressionSub tk _ _  -> tk
      ExpressionStrCat tk _ _ -> tk
      ExpressionEq tk _ _   -> tk
      ExpressionGt tk _ _   -> tk
      ExpressionGtEq tk _ _ -> tk
      ExpressionAnd tk _ _  -> tk
      ExpressionOr tk _ _   -> tk
      ExpressionNull tk     -> tk
      ExpressionNoop        -> fakeToken
