{
import Oodle.Token

module Oodle (
  Parser(..),
  Exp(..),
  parseError
) where

}

%name Parser
%tokentype { Oodle.Token }
%error { parseError }

%token
  -- Comments
  '~'       { TokenComment }

  -- Newline
  '\n'      { TokenNewline }

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

Exp   : id        { TokenIdentifier $1 }
      | strLit    { TokenStringLiteral $1 }
      | intLit    { TokenIntLiteral $1 }

{

parseError :: [Token] -> a
parseError a = error $ " Parse error:" ++ (show a)

data Exp
      = Int Int
      | String String
      | Exp Exp
  deriving Show
}
