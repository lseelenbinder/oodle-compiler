-- Filename: oodle.y
-- Contents: The directies for the Happy compiler-compiler.
-- Notes: This code probably does not work for Phase 1, as it is not needed.

{
module Oodle.Parser where

import Oodle.Token
import Oodle.ParseTree
import Data.List.Split (splitOn)
import Oodle.Error

}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Error }
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
  intLit    { Token (TokenIntLiteral _) _ }
  strLit    { Token (TokenStringLiteral _) _ }
  id        { Token (TokenIdentifier _) _ }

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

Start         : nullable_cr ClassList           { $$ = Start $2 }
nullable_cr   : {- empty -}                     { }
              | cr                              { }

-- Classes
ClassList     : Class nullable_cr               { $$ = [$1] }
              | Class cr ClassList              { $$ = $1 : $3 }

Class
              : class Id InheritsExpr is cr
                VarList
                MethodList
                Id                              { $$ = Class $1 $2 $3 (reverse $6) $7;
                where if $2 == $8 then return $2 else failWithToken $1 "Class closing name not equal to beginning name."}

Id            : id                              { $$ = (Id (getTokenStr $1)); $$.tk = $1 }
InheritsExpr
              : inherits from Id                { $$ = $3 }
              | {- empty -}                     { $$ = Id "" }

-- Methods
MethodList
              : Method MethodList               { $$ = $1 : $2 }
              | end                             { $$ = [] }

Method        : Id '(' ArgumentList ')' TypeExpression is cr
                VarList
                begin cr
                StatementList
                end Id cr                       { $$ = Method $1.tk $1 $5 $3 (reverse $8) $11
                ; where if $1 == $13 then return $1 else failWithToken $2 "Method closing name not equal to beginning name."
                }

-- Variables
VarList
              : VarList VarDecl                 { $$ = ($2 : $1) }
              | {- empty -}                     { $$ = [] }

VarDecl       : Id TypeExpression NullableInit cr
                                                { $$ = Var $1.tk $1 $2 $3}

TypeExpression
              : ':' Type                        { $$ = $2 }
              | {- empty -}                     { $$ = typeNull }
NullableInit  :
              InitExpression                    { $$ = $1 }
              | {- empty -}                     { $$ = ExpressionNoop }
InitExpression
              : ':=' Expression                 { $$ = $2 }
-- Arguments
ArgumentList
              : ArgumentList ';' Argument       { $$ = $1 ++ [$3] }
              | Argument                        { $$ = [$1] }
              | {- empty -}                     { $$ = [] }

Argument      : Id ':' Type                     { $$ = Argument $1.tk $1 $3 }

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
              : Id ArrayIndexList InitExpression{
              $$ = if $2 == [] then
                AssignStatement $1.tk $1 $3
              else
                AssignStatement $1.tk (IdArray (getIdString $1) $2) $3
              }

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
                end loop                        { $$ = LoopStatement $1 $3 $5 }

-- Call
CallStatement
              : CallScope '(' CallExprList ')'  { $$ = CallStatement $1.tk (fst $1) (snd $1) $3 }
CallScope
              : Expression '.' Id               { $$ = ($1, $3); $$.tk = $2 }
              | Id                              { $$ = (ExpressionNoop, $1); $$.tk = $1.tk }
CallExprList
              : ExpressionList                  { $$ = $1; $1.tk = $$.tk }
              | {- empty -}                     { $$ = [] }


-- Array Indexing
ArrayIndexList
              : {- empty -}                     { $$ = [] }
              | ArrayIndex ArrayIndexList       { $$ = $1 : $2 }
ArrayIndex
              : '[' Expression ']'              { $$ = $2 }
ArrayIndexOptExpr
              : '[' Expression ']'              { $$ = $2 }
              | '[' ']'                         { $$ = ExpressionNoop }


-- Type
Type          : int                             { $$ = TypeInt }
              | string                          { $$ = typeString }
              | boolean                         { $$ = TypeBoolean }
              | Id                              { $$ = TypeId $1 }
              | Type ArrayIndexOptExpr          { $$ = TypeExp $1 $2 }


-- Expression
Expression
              : Id ArrayIndexList               { $$ = if null $2 then ExpressionId $1.tk $1
                                                  else ExpressionIdArray $1.tk (IdArray (getIdString $1) $2) }
              | strLit                          { $$ = ExpressionStr $1 (getTokenStr $1) }
              | intLit                          { $$ = ExpressionInt $1 (getInt (getToken $1)) }
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

ExpressionList
              : Expression                      { $$ = [$1] }
              | Expression ',' ExpressionList   { $$ = $1 : $3 }

-- one or more newlines
cr            : newline    { }
              | cr newline { }
{

failWithToken t err =
  fail $ (concatMap (\s -> s ++ ":") . init $ (splitOn ":" (printToken t))) ++ " " ++ err

parseError tokenStream = fail $ "Parse error at: " ++(
  if (length tokenStream) > 0
  then (printToken (head tokenStream))
  else "EOF")

}
