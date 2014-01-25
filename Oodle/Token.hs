-- Filename: Oodle/Token.hs
-- Contents: The datatype for Token and TokenPosition.

module Oodle.Token where

-- TokenPosition holds the file, line, and column # of a found token
data TokenPosition = TokenPosition Token FilePath Int Int deriving (Show)

-- Returns the Token portion of a TokenPosition record.
getToken :: TokenPosition -> Token
getToken (TokenPosition t _ _ _) = t
-- Returns the file path portion of a TokenPosition record.
getFilePath :: TokenPosition -> FilePath
getFilePath (TokenPosition _ file _ _) = file
-- Returns the line portion of a TokenPosition record.
getLine :: TokenPosition -> Int
getLine (TokenPosition _ _ l _) = l
-- Returns the column portion of a TokenPosition record.
getCol :: TokenPosition -> Int
getCol (TokenPosition _ _ _ c) = c
-- Returns a formated string representing a TokenPosition record.
printToken :: TokenPosition -> String
printToken (TokenPosition t file line col) = file ++ ":" ++ show line ++ "," ++ show col ++ ":" ++ show t

-- Token stores the various types of tokens and any data associated with them.
data Token
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

-- returns true if the token is the result of a lexical error
isErrorToken :: Token -> Bool
isErrorToken (TokenInvalid _) = True
isErrorToken (TokenUnterminatedString _) = True
isErrorToken (TokenInvalidString _) = True
isErrorToken _ = False
