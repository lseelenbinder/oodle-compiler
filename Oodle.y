-- Filename: oodle.y
-- Contents: The directies for the Happy compiler-compiler.
-- Notes: This code probably does not work for Phase 1, as it is not needed.

{
module Oodle.Parser (
  parser
) where

import Oodle.Token

}

%name parser
%tokentype { Token }
%error { parseError }
%left 'or'
%left 'and'
%nonassoc '=' '>' '>='
%left '&'
%left '+' '-'
%left '*' '/'
%right UNARY
%right '.' new

%token
  -- Newline
  newline      { TokenNewline }

  -- Literals
  intLit    { TokenIntLiteral $$ }
  strLit    { TokenStringLiteral $$ }
  id        { TokenIdentifier $$ }

  -- Keywords
  boolean   { TokenBoolean }
  begin     { TokenBegin }
  class     { TokenClass }
  else      { TokenElse }
  end       { TokenEnd }
  false     { TokenFalse }
  from      { TokenFrom }
  if        { TokenIf }
  inherits  { TokenInherits }
  int       { TokenInt }
  is        { TokenIs }
  loop      { TokenLoop }
  me        { TokenMe }
  new       { TokenNew }
  not       { TokenNot }
  null      { TokenNull }
  string    { TokenString }
  then      { TokenThen }
  true      { TokenTrue }
  while     { TokenWhile }
  and       { TokenAnd }
  or        { TokenOr }

  -- Operators
  '&'       { TokenStringConcat }
  '+'       { TokenPlus }
  '-'       { TokenMinus }
  '*'       { TokenTimes }
  '/'       { TokenDiv }
  '>'       { TokenGT }
  '>='      { TokenGTEq }
  '='       { TokenEq }

  -- Miscellaneous
  ':='      { TokenAssign }
  '('       { TokenOP }
  ')'       { TokenCP }
  '['       { TokenOB }
  ']'       { TokenCB }
  ','       { TokenComma }
  ';'       { TokenSemicolon }
  ':'       { TokenColon }
  '.'       { TokenPeriod }

%%

Start     : cr ClassList                        { Start $2 }

-- Classes
ClassList     ::                                { [Class] }
ClassList     : Class                           { [$1] }
              | Class ClassList                 { $1 : $2 }

Class         : class Id InheritsExpr is cr
                VarList
                MethodList
                Id cr                           { Class $2 $3 $6 $7 $8 }

InheritsExpr  ::                                { Id }
InheritsExpr  : inherits from Id                { $> }
              | {- empty -}                     { Id "" }

-- Methods
MethodList    ::                                { [Method] }
              : Method MethodList               { $1 : $2 }
              | end                             { [] }

Method        : Id '(' ArgumentList ')' is cr
                VarList
                begin cr
                StatementList
                end Id cr                       { Method $1 $3 $7 $10 $12 }

-- Variables
VarList       ::                                { [Var] }
              : VarList Var                     { (concat [$1, [$2]]) }
              | {- empty -}                     { [] :: [Var] }

Var           : Id TypeExpression InitExpression cr { Var $1 $2 $3}

TypeExpression : ':' Type                       { $2 }
               | {- empty -}                    { TypeNull }
InitExpression : ':=' Expression                { $2 }
               | {- empty -}                    { ExpressionNull }
-- Arguments
ArgumentList  ::                                { [Argument] }
              : ArgumentList ';' Argument       { $3 : $1 }
              | Argument                        { [$1] }
              | {- empty -}                     { [] }

Argument      : Id ':' Type                     { Argument $1 $3 }

-- Statements
StatementList ::                                { [Statement] }
              : {- empty -}                     { [] }
              | Statement cr StatementList         { $1 : $3 }

Statement     ::                                { Statement }
Statement     : AssignStatement                 { $1 }
              | IfStatement                     { $1 }
              | LoopStatement                   { $1 }
              | CallStatement                   { $1 }

-- Assign
AssignStatement ::                              { Statement }
              : id ArrayIndexList InitExpression{ AssignStatement (IdArray $1 $2) $> }

-- If
IfStatement   ::                                { Statement }
              : if Expression then cr
                StatementList
                ElseClause
                end if                          { IfStatement $2 $5 $6 }
ElseClause    ::                                { [Statement] }
              : else cr
                StatementList                   { $> }
              | {- empty -}                     { [] }

-- Loop
LoopStatement ::                                { Statement }
              : loop while Expression cr
                StatementList
                end loop                        { LoopStatement $3 $5 }

-- Call
CallStatement ::                                { Statement }
              : CallScope '(' CallExprList ')'  { CallStatement (fst $1) (snd $1) $3 }
CallScope     ::                                { (Expression, Id) }
              : Expression '.' Id               { ($1, $3) }
              | Id                              { (ExpressionNull, $1) }
CallExprList  ::                                { [Expression] }
              : ExpressionList                  { $1 }
              | {- empty -}                     { [] }


-- Array Indexing
ArrayIndexList: {- empty -}                     { [] }
              | ArrayIndexList ArrayIndex       { $2 : $1 }
ArrayIndex    : '[' Expression ']'              { $2 }


-- Type
Type          : int                             { TypeInt }
              | string                          { TypeString }
              | boolean                         { TypeBoolean }
              | Id                              { TypeId $1 }
              | Type ArrayIndex                 { TypeExp $1 $2 }


-- Expression
Expression    ::                                { Expression }
              : Id                              { ExpressionId $1 }
              | strLit                          { ExpressionStr $1 }
              | intLit                          { ExpressionInt $1 }
              | true                            { ExpressionTrue }
              | false                           { ExpressionFalse }
              | null                            { ExpressionNull }
              | me                              { ExpressionMe }
              | new Type                        { ExpressionType $2 }
              | '(' Expression ')'              { $2 }
              | CallScope '(' CallExprList ')'  { ExpressionCall (fst $1) (snd $1) $3 }

              -- Operators
              | not Expression %prec UNARY      { ExpressionNot $2 }
              | '-' Expression %prec UNARY      { ExpressionNeg $2 }
              | '+' Expression %prec UNARY      { ExpressionPos $2 }
              | Expression '*' Expression       { ExpressionMul $1 $3 }
              | Expression '/' Expression       { ExpressionDiv $1 $3 }
              | Expression '+' Expression       { ExpressionAdd $1 $3 }
              | Expression '-' Expression       { ExpressionSub $1 $3 }
              | Expression '&' Expression       { ExpressionStrCat $1 $3 }
              | Expression '=' Expression       { ExpressionEq $1 $3 }
              | Expression '>' Expression       { ExpressionGt $1 $3 }
              | Expression '>=' Expression      { ExpressionGtEq $1 $3 }
              | Expression and Expression       { ExpressionAnd $1 $3 }
              | Expression or Expression        { ExpressionOr $1 $3 }

              | id ArrayIndex ArrayIndexList    { ExpressionIdArray (IdArray $1 ($2 : $3)) }

ExpressionList::                                { [Expression] }
              : Expression                      { [$1] }
              | Expression ',' ExpressionList   { $1 : $3 }

-- one or more newlines
cr            : newline    { }
              | cr newline { }

Id            : id                              { Id $1 }
{

parseError :: [Token] -> a
parseError tokenStream = error $ " Parse error at: " ++ (show (head tokenStream))

data Start
      = Start [Class]
  deriving Show

data Class
      = Class Id Id [Var] [Method] Id
  deriving Show

data Var
      = Var Id Type Expression
  deriving Show

data Method
      = Method Id [Argument] [Var] [Statement] Id
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
  deriving Show

data Type
      = TypeInt
      | TypeNull
      | TypeString
      | TypeBoolean
      | TypeId Id
      | TypeExp Type Expression
  deriving Show

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
  deriving Show
}
