-- Filename: oodle.y
-- Contents: The directies for the Happy compiler-compiler.
-- Notes: This code probably does not work for Phase 1, as it is not needed.

{
module Oodle.Parser where

import Oodle.Token
import Oodle.ParseTree
import Data.List.Split (splitOn)

}

%name parser
%tokentype { Token }
%error { parseError }
%monad { E }
%left or
%left and
%nonassoc '=' '>' '>='
%left '&'
%left '+' '-'
%left '*' '/'
%right UNARY
%right '.' new

%token
  -- Newline
  newline      { Token TokenNewline _ }

  -- Literals
  intLit    { Token (TokenIntLiteral $$) _ }
  strLit    { Token (TokenStringLiteral $$) _ }
  id        { Token (TokenIdentifier $$) _ }

  -- Keywords
  boolean   { Token TokenBoolean _ }
  begin     { Token TokenBegin _ }
  class     { Token TokenClass _ }
  else      { Token TokenElse _ }
  end       { Token TokenEnd _ }
  false     { Token TokenFalse _ }
  from      { Token TokenFrom _ }
  if        { Token TokenIf _ }
  inherits  { Token TokenInherits _ }
  int       { Token TokenInt _ }
  is        { Token TokenIs _ }
  loop      { Token TokenLoop _ }
  me        { Token TokenMe _ }
  new       { Token TokenNew _ }
  not       { Token TokenNot _ }
  null      { Token TokenNull _ }
  string    { Token TokenString _ }
  then      { Token TokenThen _ }
  true      { Token TokenTrue _ }
  while     { Token TokenWhile _ }
  and       { Token TokenAnd _ }
  or        { Token TokenOr _ }

  -- Operators
  '&'       { Token TokenStringConcat _ }
  '+'       { Token TokenPlus _ }
  '-'       { Token TokenMinus _ }
  '*'       { Token TokenTimes _ }
  '/'       { Token TokenDiv _ }
  '>'       { Token TokenGT _ }
  '>='      { Token TokenGTEq _ }
  '='       { Token TokenEq _ }

  -- Miscellaneous
  ':='      { Token TokenAssign _ }
  '('       { Token TokenOP _ }
  ')'       { Token TokenCP _ }
  '['       { Token TokenOB _ }
  ']'       { Token TokenCB _ }
  ','       { Token TokenComma _ }
  ';'       { Token TokenSemicolon _ }
  ':'       { Token TokenColon _ }
  '.'       { Token TokenPeriod _ }

%%

Start         : cr ClassList                    { Start $2 }

-- Classes
ClassList     ::                                { [Class] }
ClassList     : Class                           { [$1] }
              | Class cr                        { [$1] }
              | Class cr ClassList              { $1 : $3 }

Class         : class id InheritsExpr is cr
                VarList
                MethodList
                id                              { Class (Id $2) $3 $6 $7 (Id $8) }

InheritsExpr  ::                                { Id }
InheritsExpr  : inherits from id                { Id $> }
              | {- empty -}                     { Id "" }

-- Methods
MethodList    ::                                { [Method] }
              : Method MethodList               { $1 : $2 }
              | end                             { [] }

Method        : id '(' ArgumentList ')' TypeExpression is cr
                VarList
                begin cr
                StatementList
                end id cr                       { Method (Id $1) $5 $3 $8 $11 (Id $13) }

-- Variables
VarList       ::                                { [Var] }
              : VarList Var                     { (concat [$1, [$2]]) }
              | {- empty -}                     { [] :: [Var] }

Var           : id TypeExpression InitExpression cr { Var (Id $1) $2 $3}

TypeExpression : ':' Type                       { $2 }
               | {- empty -}                    { TypeNull }
InitExpression : ':=' Expression                { $2 }
               | {- empty -}                    { ExpressionNull }
-- Arguments
ArgumentList  ::                                { [Argument] }
              : ArgumentList ';' Argument       { $3 : $1 }
              | Argument                        { [$1] }
              | {- empty -}                     { [] }

Argument      : id ':' Type                     { Argument (Id $1) $3 }

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
              : Expression '.' id               { ($1, (Id $3)) }
              | id                              { (ExpressionNull, (Id $1)) }
CallExprList  ::                                { [Expression] }
              : ExpressionList                  { $1 }
              | {- empty -}                     { [] }


-- Array Indexing
ArrayIndexList
              : {- empty -}                     { [] }
              | ArrayIndex ArrayIndexList       { $1 : $2 }
ArrayIndex
              : '[' Expression ']'              { $2 }
ArrayIndexOptExpr
              : '[' Expression ']'              { $2 }
              | '[' ']'                         { ExpressionNull }


-- Type
Type          : int                             { TypeInt }
              | string                          { TypeString }
              | boolean                         { TypeBoolean }
              | id                              { TypeId (Id $1) }
              | Type ArrayIndexOptExpr          { TypeExp $1 $2 }


-- Expression
Expression    ::                                { Expression }
              : id                              { ExpressionId (Id $1) }
              | strLit                          { ExpressionStr $1 }
              | intLit                          { ExpressionInt $1 }
              | true                            { ExpressionTrue }
              | false                           { ExpressionFalse }
              | null                            { ExpressionNull }
              | me                              { ExpressionMe }
              | new Type                        { ExpressionType $2 }
              | '(' Expression ')'              { $2 }
              | CallScope '(' CallExprList ')'  { ExpressionCall (fst $1) (snd $1) $3 }

              -- Unary Operators
              | not Expression %prec UNARY      { ExpressionNot $2 }
              | '-' Expression %prec UNARY      { ExpressionNeg $2 }
              | '+' Expression %prec UNARY      { ExpressionPos $2 }

              -- Binary Operators
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
{

data E a = Ok a | Failed String | SemanticFail String
  deriving (Show)

instance Monad E where
  (>>=) m k =
    case m of
      Ok a -> k a
      SemanticFail e -> SemanticFail e
      Failed e -> Failed e
  return a  = Ok a
  fail err  = Failed err

failWithToken t err =
  SemanticFail $ (concatMap (\s -> s ++ ":") . init $ (splitOn ":" (printToken t))) ++ " " ++ err

parseError tokenStream = fail $ "Parse error at: " ++(
  if (length tokenStream) > 0
  then (printToken (head tokenStream))
  else "EOF")

}
