-- Filename: Oodle/Token.hs
-- Contents: The datatype for Token and TokenPosition.
module Oodle.Token where

-- TokenPosition holds the file, line, and column # of a found token
data TokenPosition = TokenPosition FilePath Int Int
  deriving (Show, Eq)

-- Returns the file path portion of a TokenPosition record.
getFilePath :: TokenPosition -> FilePath
getFilePath (TokenPosition file _ _) = file
-- Returns the line portion of a TokenPosition record.
getLine :: TokenPosition -> Int
getLine (TokenPosition _ l _) = l
-- Returns the column portion of a TokenPosition record.
getCol :: TokenPosition -> Int
getCol (TokenPosition _ _ c) = c
-- Returns a formated string representing a TokenPosition record.
printToken :: Token -> String
printToken (Token t (TokenPosition file line col)) =
  file ++ ":" ++ show line ++ "," ++ show col ++ ":" ++ show t

-- Token is a wrapper for a literal Token and its Position
data Token = Token TokenType TokenPosition
  deriving (Show, Eq)

-- TokenType stores the various types of tokens and any data associated with them.
data TokenType
      = TokenIntLiteral Int
      | TokenNewline
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
      | TokenInvalidString String
      -- Placeholder tokens for the lexer
      | NilToken
      | NilNewlineToken
  deriving (Show, Eq)

-- returns the actual token
getToken :: Token -> TokenType
getToken (Token t _) = t

-- returns true if the token is the result of a lexical error
isErrorToken :: Token -> Bool
isErrorToken t = isErrorToken' (getToken t)

isErrorToken' :: TokenType -> Bool
isErrorToken' (TokenInvalid _) = True
isErrorToken' (TokenUnterminatedString _) = True
isErrorToken' (TokenInvalidString _) = True
isErrorToken' _ = False
