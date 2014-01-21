module Oodle.Token ( Token(..) ) where

data Token
      = TokenIntLiteral Int
      | TokenNewline
      | TokenComment
      | TokenStringLiteral String
      | TokenIdentifier String
      | TokenBoolean
      | TokenBegin
      | TokenClass
      | TokenElse
      | TokenEnd
      | TokenFalse
      | TokenFrom
      | TokenIf
      | TokenInherits
      | TokenInt
      | TokenIs
      | TokenLoop
      | TokenMe
      | TokenNew
      | TokenNot
      | TokenNull
      | TokenString
      | TokenThen
      | TokenTrue
      | TokenWhile
      | TokenAnd
      | TokenOr
      | TokenStringConcat
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenGT
      | TokenGTEq
      | TokenEq
      | TokenAssign
      | TokenOP
      | TokenCP
      | TokenOB
      | TokenCB
      | TokenComma
      | TokenSemicolon
      | TokenColon
      | TokenPeriod
      | TokenInvalid Char
      | TokenUnterminatedString String
      | TokenInvalidEscape Char
  deriving (Show)

