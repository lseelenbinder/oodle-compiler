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
%attributetype { Attrs a }
%attribute value { a }
%attribute tk { Token }

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

Start         : cr ClassList                    { $$ = Start $2 }

-- Classes
ClassList
              : Class                           { $$ = [$1] }
              | Class cr                        { $$ = [$1] }
              | Class cr ClassList              { $$ = $1 : $3 }

Class
              : class id InheritsExpr is cr
                VarList
                MethodList
                id                              { $$ = Class $1 (Id $2) $3 $6 $7 }

InheritsExpr
              : inherits from id                { $$ = Id $3 }
              | {- empty -}                     { $$ = Id "" }

-- Methods
MethodList
              : Method MethodList               { $$ = $1 : $2 }
              | end                             { $$ = [] }

Method        : id '(' ArgumentList ')' TypeExpression is cr
                VarList
                begin cr
                StatementList
                end id cr                       { $$ = Method $2 (Id $1) $5 $3 $8 $11 }

-- Variables
VarList
              : VarList VarDecl                 { $$ = reverse ($2 : $1) }
              | {- empty -}                     { $$ = [] }

VarDecl       : id TypeExpression NullableInit cr
                                                { $$ = Var (Id $1) $2 $3}

TypeExpression
              : ':' Type                        { $$ = $2 }
              | {- empty -}                     { $$ = TypeNull }
NullableInit  :
              InitExpression                    { $$ = $1 }
              | {- empty -}                     { $$ = ExpressionNoop }
InitExpression
              : ':=' Expression                 { $$ = $2; $$.tk = $1; $2.tk = $1}
-- Arguments
ArgumentList
              : ArgumentList ';' Argument       { $$ = $3 : $1 }
              | Argument                        { $$ = [$1] }
              | {- empty -}                     { $$ = [] }

Argument      : id ':' Type                     { $$ = Argument (Id $1) $3 }

-- Statements
StatementList
              : {- empty -}                     { $$ = [] }
              | Statement cr StatementList      { $$ = $1 : $3 }

Statement
              : AssignStatement                 { $$ = $1 }
              | IfStatement                     { $$ = $1 }
              | LoopStatement                   { $$ = $1 }
              | CallStatement                   { $$ = $1 }

-- Assign
AssignStatement
              : id ArrayIndexList InitExpression{ $$ = AssignStatement $3.tk (IdArray $1 $2) $3 }

-- If
IfStatement
              : if Expression then cr
                StatementList
                ElseClause
                end if                          { $$ = IfStatement $1 $2 $5 $6; $2.tk = $1 }
ElseClause
              : else cr
                StatementList                   { $$ = $3 }
              | {- empty -}                     { $$ = [] }

-- Loop
LoopStatement
              : loop while Expression cr
                StatementList
                end loop                        { $$ = LoopStatement $1 $3 $5; $3.tk = $2 }

-- Call
CallStatement
              : CallScope '(' CallExprList ')'  { $$ = CallStatement $2 (fst $1) (snd $1) $3; $3.tk = $2 }
CallScope
              : Expression '.' id               { $$ = ($1, (Id $3)); $1.tk = $2 }
              | id                              { $$ = (ExpressionNoop, (Id $1)) }
CallExprList
              : ExpressionList                  { $$ = $1; $1.tk = $$.tk }
              | {- empty -}                     { $$ = [] }


-- Array Indexing
ArrayIndexList
              : {- empty -}                     { $$ = [] }
              | ArrayIndex ArrayIndexList       { $$ = $1 : $2 }
ArrayIndex
              : '[' Expression ']'              { $$ = $2; $2.tk = $1 }
ArrayIndexOptExpr
              : '[' Expression ']'              { $$ = $2; $2.tk = $1 }
              | '[' ']'                         { $$ = ExpressionNoop }


-- Type
Type          : int                             { $$ = TypeInt }
              | string                          { $$ = TypeString }
              | boolean                         { $$ = TypeBoolean }
              | id                              { $$ = TypeId (Id $1) }
              | Type ArrayIndexOptExpr          { $$ = TypeExp $1 $2 }


-- Expression
Expression
              : id                              { $$ = ExpressionId (Id $1) }
              | strLit                          { $$ = ExpressionStr $1 }
              | intLit                          { $$ = ExpressionInt $1 }
              | true                            { $$ = ExpressionTrue $1 }
              | false                           { $$ = ExpressionFalse $1 }
              | null                            { $$ = ExpressionNull $1 }
              | me                              { $$ = ExpressionMe $1 }
              | new Type                        { $$ = ExpressionNew $1 $2 }
              | '(' Expression ')'              { $$ = $2 }
              | CallScope '(' CallExprList ')'  { $$ = ExpressionCall $2 (fst $1) (snd $1) $3 }

              -- Unary Operators
              | not Expression %prec UNARY      { $$ = ExpressionNot $1 $2 }
              | '-' Expression %prec UNARY      { $$ = ExpressionNeg $1 $2 }
              | '+' Expression %prec UNARY      { $$ = ExpressionPos $1 $2 }

              -- Binary Operators
              | Expression '*' Expression       { $$ = ExpressionMul $2 $1 $3 }
              | Expression '/' Expression       { $$ = ExpressionDiv $2 $1 $3 }
              | Expression '+' Expression       { $$ = ExpressionAdd $2 $1 $3 }
              | Expression '-' Expression       { $$ = ExpressionSub $2 $1 $3 }
              | Expression '&' Expression       { $$ = ExpressionStrCat $2 $1 $3 }
              | Expression '=' Expression       { $$ = ExpressionEq $2 $1 $3 }
              | Expression '>' Expression       { $$ = ExpressionGt $2 $1 $3 }
              | Expression '>=' Expression      { $$ = ExpressionGtEq $2 $1 $3 }
              | Expression and Expression       { $$ = ExpressionAnd $2 $1 $3 }
              | Expression or Expression        { $$ = ExpressionOr $2 $1 $3 }

              | id ArrayIndex ArrayIndexList    { $$ = ExpressionIdArray $$.tk (IdArray $1 ($2 : $3)) }

ExpressionList
              : Expression                      { $$ = [$1]; $1.tk = $$.tk }
              | Expression ',' ExpressionList   { $$ = $1 : $3; $1.tk = $$.tk; $2.tk = $$.tk }

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
