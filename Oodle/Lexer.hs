module Oodle.Lexer (
  lexer
  ) where

import Oodle.Token
import Data.Char

-- The lexer takes a string and parses it according to the Oodle tokens.
lexer :: String -> [TokenPosition]
lexer cs =
  lexer' cs 1 1

lexer' :: String -> Int -> Int -> [TokenPosition]
lexer' [] _ _ = []
lexer' cs' line col =
  let
    new_col = col + ((length cs') - (length rest))
  in
    if token == NilToken
    then
      lexer' rest line new_col
    else
      if token == NilNewlineToken
      then
        lexer' rest (line + 1) 1
      else
        TokenPosition token line col : (if token == TokenNewline then
          lexer' rest (line + 1) 1
          else
          lexer' rest line new_col
          )
  where (token, rest) = lexSymbol cs'

-- Takes a string and returns the next token and the remaining text to parse
lexSymbol :: String -> (Token, String)
lexSymbol ('~':cs)      = lexComment cs
lexSymbol ('_':'\n':cs) = (NilNewlineToken, cs)
lexSymbol ('\n':cs)     = (TokenNewline, cs)
lexSymbol ('&':cs)      = (TokenStringConcat, cs)
lexSymbol ('+':cs)      = (TokenPlus, cs)
lexSymbol ('-':cs)      = (TokenMinus, cs)
lexSymbol ('*':cs)      = (TokenTimes, cs)
lexSymbol ('/':cs)      = (TokenDiv, cs)
lexSymbol ('>':cs)      = lexGT cs
lexSymbol ('=':cs)      = (TokenEq, cs)
lexSymbol (':':cs)      = lexColon cs
lexSymbol ('(':cs)      = (TokenOP, cs)
lexSymbol (')':cs)      = (TokenCP, cs)
lexSymbol ('[':cs)      = (TokenOB, cs)
lexSymbol (']':cs)      = (TokenCB, cs)
lexSymbol (',':cs)      = (TokenComma, cs)
lexSymbol (';':cs)      = (TokenSemicolon, cs)
lexSymbol ('.':cs)      = (TokenPeriod, cs)
lexSymbol ('"':cs)      = lexString cs
lexSymbol (c:cs)
  | isDigit c = lexNum (c:cs)
  | isSpace c = (NilToken, cs) -- ignore whitespace
  | isAlpha c || c == '_' = lexId (c:cs)
  | otherwise = (TokenInvalid c, cs)

-- lex identifiers and keywords
lexId :: String -> (Token, String)
lexId cs =
    (lexId' identifier, rest)
  where (identifier, rest) = span (\x -> isAlphaNum x || x == '_') cs

lexId' :: String -> Token
-- Keywords
lexId' "and"      = TokenAnd
lexId' "boolean"  = TokenBoolean
lexId' "begin"    = TokenBegin
lexId' "class"    = TokenClass
lexId' "else"     = TokenElse
lexId' "false"    = TokenFalse
lexId' "from"     = TokenFrom
lexId' "if"       = TokenIf
lexId' "inherits" = TokenInherits
lexId' "int"      = TokenInt
lexId' "is"       = TokenIs
lexId' "loop"     = TokenLoop
lexId' "me"       = TokenMe
lexId' "new"      = TokenNew
lexId' "not"      = TokenNot
lexId' "null"     = TokenNull
lexId' "or"       = TokenOr
lexId' "string"   = TokenString
lexId' "then"     = TokenThen
lexId' "true"     = TokenTrue
lexId' "while"    = TokenWhile
-- Identifiers
lexId' (identifier) = TokenIdentifier identifier


-- lex number literals
lexNum :: String -> (Token, String)
lexNum cs = (TokenIntLiteral (read num), rest)
  where (num,rest) = span isDigit cs

-- lex comments (i.e., skip them)
lexComment :: String -> (Token, String)
lexComment cs = (NilToken, rest)
  where (_, rest) = span (/= '\n') cs

-- lex greater than and greater than equal to
lexGT :: String -> (Token, String)
lexGT ('=':cs) = (TokenGTEq, cs)
lexGT (cs) = (TokenGT, cs)

-- lex colon and assignment
lexColon :: String -> (Token, String)
lexColon ('=':cs) = (TokenAssign, cs)
lexColon (cs) = (TokenColon, cs)

-- lex string literals (and catch errors
-- \xBAD denotes an unterminated string
-- \xBED denotes an invalid string
lexString :: String -> (Token, String)
lexString cs =
  let
    rest = drop (length str) cs
    actual = init str
  in
    if last str == '\xBAD' || last str == '\xBFD'
    then
      (TokenUnterminatedString actual,
        (if last str == '\xBFD' then '\n' : rest else rest))
    else
      if last str == '\xBED'
      then
        (TokenInvalidString actual, rest)
      else
        (TokenStringLiteral actual, rest)
  where str = scanUntilDouble cs

-------------------
-- Helper Methods--
-------------------

scanUntilDouble :: String -> String
-- \xBAD denotes an unterminated string
scanUntilDouble [] = "\xBAD"
-- \xBAD denotes an unterminated string due to a newline
scanUntilDouble('\n':_) = "\xBFD"
scanUntilDouble('"':_) = ['"'] -- I'm done parsing the string.
-- TODO: only accept three character octal sequences
scanUntilDouble ('\\':cs) =
  if (length octal) > 0
  then
    if (length octal) /= 3
    then
      concat ["\\", octal, scanUntilDouble (drop (length octal) cs), "\xBED"]
    else
      '\\' : octal ++ (scanUntilDouble $ drop 3 cs)
  else
    if ((head cs) `elem` ['t', 'n', 'f', 'r', '"', '\\'])
    then
      '\\' : (head cs) : scanUntilDouble (tail cs)
    else
      concat ["\\", (head cs) : [], scanUntilDouble (tail cs), "\xBED"]
  where (octal, rest) = span isOctDigit cs
scanUntilDouble(c:cs) = c:scanUntilDouble cs
