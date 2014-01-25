module Oodle.Token where

-- TokenPosition holds the file, line, and column # of a found token
data TokenPosition = TokenPosition Token FilePath Int Int deriving (Show)
getToken :: TokenPosition -> Token
getToken (TokenPosition t _ _ _) = t
getFilePath :: TokenPosition -> FilePath
getFilePath (TokenPosition _ file _ _) = file
getLine :: TokenPosition -> Int
getLine (TokenPosition _ _ l _) = l
getCol :: TokenPosition -> Int
getCol (TokenPosition _ _ _ c) = c
printToken :: TokenPosition -> String
printToken (TokenPosition t file line col) = file ++ ":" ++ show line ++ "," ++ show col ++ ":" ++ show t

-- Token stores the varis
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

isErrorToken :: Token -> Bool
isErrorToken (TokenInvalid _) = True
isErrorToken (TokenUnterminatedString _) = True
isErrorToken (TokenInvalidString _) = True
isErrorToken _ = False
