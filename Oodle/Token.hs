-- Filename: Oodle/Token.hs
-- Contents: The datatype for Token and TokenPosition.
module Oodle.Token where

-- TokenPosition holds the file, line, and column # of a found token
data TokenPosition = TokenPosition { getFilePath :: FilePath, getLineNo ::  Int, getCol :: Int }
  deriving (Show, Eq)

printToken :: Token -> String
printToken (Token t (TokenPosition file line col)) =
  file ++ ":" ++ show line ++ "," ++ show col ++ ": " ++ show t

-- Token is a wrapper for a literal Token and its Position
data Token = Token { getToken :: TokenType, getPosition :: TokenPosition }
  deriving (Eq, Show)

-- TokenType stores the various types of tokens and any data associated with them.
data TokenType
      = TokenIntLiteral { getInt :: Int }
      | TokenNewline
      | TokenStringLiteral { getActual :: String }
      | TokenIdentifier { getActual :: String }
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
      | TokenInvalid { getChar :: Char }
      | TokenUnterminatedString { getActual :: String }
      | TokenInvalidString { getActual :: String }
      -- Placeholder tokens for the lexer
      | NilToken
      | NilNewlineToken
  deriving (Eq)

instance Show TokenType where
  show (tk) =
    case tk of
      TokenIntLiteral i -> show i
      TokenNewline -> "newline"
      TokenStringLiteral str -> str
      TokenIdentifier str -> str
      TokenBoolean      -> "boolean"
      TokenBegin        -> "begin"
      TokenClass        -> "class"
      TokenElse         -> "else"
      TokenEnd          -> "end"
      TokenFalse        -> "false"
      TokenFrom         -> "from"
      TokenIf           -> "if"
      TokenInherits     -> "inherits"
      TokenInt          -> "int"
      TokenIs           -> "is"
      TokenLoop         -> "loop"
      TokenMe           -> "me"
      TokenNew          -> "new"
      TokenNot          -> "not"
      TokenNull         -> "null"
      TokenString       -> "string"
      TokenThen         -> "then"
      TokenTrue         -> "true"
      TokenWhile        -> "while"
      TokenAnd          -> "and"
      TokenOr           -> "or"
      TokenStringConcat -> "&"
      TokenPlus         -> "+"
      TokenMinus        -> "-"
      TokenTimes        -> "*"
      TokenDiv          -> "/"
      TokenGT           -> ">"
      TokenGTEq         -> ">="
      TokenEq           -> "="
      TokenAssign       -> ":="
      TokenOP           -> "("
      TokenCP           -> ")"
      TokenOB           -> "["
      TokenCB           -> "]"
      TokenComma        -> ","
      TokenSemicolon    -> ";"
      TokenColon        -> ":"
      TokenPeriod       -> "."
      TokenInvalid c    -> show c
      TokenUnterminatedString str -> "Unterminated: " ++ str
      TokenInvalidString str -> "Invalid: " ++ str
      NilToken          -> "nilToken"
      NilNewlineToken   -> "nilNewlineToken"

getTokenStr :: Token -> String
getTokenStr = getActual . getToken

-- returns true if the token is the result of a lexical error
isErrorToken :: Token -> Bool
isErrorToken t =
  case getToken t of
    TokenInvalid _            -> True
    TokenUnterminatedString _ -> True
    TokenInvalidString _      -> True
    _                         -> False

fakeToken :: Token
fakeToken = Token TokenNull (TokenPosition "" 0 0)
