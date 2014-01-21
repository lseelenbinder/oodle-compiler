module Oodle.Lexer (
  lexer
  ) where

import Oodle.Token
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer ('~':cs) = lexComment (cs)
lexer ('_':'\n':cs) = lexer cs
lexer ('\n':cs) = TokenNewline : lexer cs
lexer ('&':cs) = TokenStringConcat : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('>':cs) = lexGT cs
lexer ('=':cs) = TokenEq : lexer cs
lexer (':':cs) = lexColon cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('.':cs) = TokenPeriod : lexer cs
lexer ('"':cs) = lexString (cs)
lexer (c:cs)
  | isDigit c = lexNum (c:cs)
  | isSpace c = lexer cs -- ignore whitespace
  | (\x -> (or [isAlpha x, (==) '_' x])) c = lexId (c:cs)
  | otherwise = TokenInvalid c : lexer cs

lexGT ('=':cs) = TokenGTEq : lexer cs
lexGT (cs) = TokenGT : lexer cs

lexNum cs = TokenIntLiteral (read num) : lexer rest
  where (num,rest) = span isDigit cs

lexComment cs = lexer rest
  where (_, rest) = span (/= '\n') cs

lexString cs = if (head rest) /= '"' then
                  TokenUnterminatedString str : lexer rest
                else
                  let
                    composed_str = string str
                  in
                    if '\xBAD' `elem` composed_str
                    then
                      (TokenInvalidEscape (last composed_str)) : lexer (tail rest)
                    else
                      (TokenStringLiteral composed_str) : lexer (tail rest)
  where (str, rest) = span (\x -> (and [(/= x) '"', (/=) x '\n'])) cs

-- Escape Sequences
string :: String -> String
string [] = []
string ('\\':code:xs) =
  case code of
    -- most of these are octal numbers
    't' -> (chr 0o11) : string xs -- tab
    'n' -> (chr 0o12) : string xs -- newline
    'f' -> (chr 0o14) : string xs -- form feed
    'r' -> (chr 0o15) : string xs -- carriage return
    '"' -> '"' : string xs
    '\\' -> (chr 0o134) : string xs -- backslash
    n -> if isOctDigit n
      then
        (chr (octalToDecimal (n:xs))) : (string (snd (octalSequence (n:xs))))
      else [(chr 0xBAD), n]
string (x:xs) = x : string xs

octalSequence str = span isOctDigit str
octalToDecimal :: String -> Int
octalToDecimal str = read ("0o" ++ fst (octalSequence (str))) :: Int

lexId cs =
  case span (\x -> or [isAlphaNum x, (==) x '_'] ) cs of
    -- Catch Keywords
    ("and", rest) -> TokenAnd : lexer rest
    ("boolean", rest) -> TokenBoolean : lexer rest
    ("begin", rest) -> TokenBegin : lexer rest
    ("class", rest) -> TokenClass : lexer rest
    ("else", rest) -> TokenElse : lexer rest
    ("false", rest) -> TokenFalse : lexer rest
    ("from", rest) -> TokenFrom : lexer rest
    ("if", rest) -> TokenIf : lexer rest
    ("inherits", rest) -> TokenInherits : lexer rest
    ("int", rest) -> TokenInt : lexer rest
    ("is", rest) -> TokenIs : lexer rest
    ("loop", rest) -> TokenLoop : lexer rest
    ("me", rest) -> TokenMe : lexer rest
    ("new", rest) -> TokenNew : lexer rest
    ("not", rest) -> TokenNot : lexer rest
    ("null", rest) -> TokenNull : lexer rest
    ("or", rest) -> TokenOr : lexer rest
    ("string", rest) -> TokenString : lexer rest
    ("then", rest) -> TokenThen : lexer rest
    ("true", rest) -> TokenTrue : lexer rest
    ("while", rest) -> TokenWhile : lexer rest

    -- Identifiers
    (id,rest) -> TokenIdentifier id : lexer rest

lexColon ('=':cs) = TokenAssign : lexer cs
lexColon (cs) = TokenColon : lexer cs

