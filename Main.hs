-- Filename: Main.hs
-- Contents: The framework for running the compiler. Parses options and runs
-- the compiler sequence.

module Main (main) where

import Oodle.Parser
import Oodle.Lexer
import Oodle.Token
import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit

-- The following functions are related to options handling and mostly copied from
-- http://hackage.haskell.org/package/base-4.6.0.1/docs/System-Console-GetOpt.html
data Options = Options  { optVerbose :: Bool, optLexer :: Bool }

startOptions :: Options
startOptions = Options  { optVerbose = False, optLexer = False }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "ds" ["debug"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Show debugging symbols"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
              prg <- getProgName
              hPutStrLn stderr (usageInfo prg options)
              exitSuccess)
              )
        "Show help"
    , Option "l" ["lexer-only"]
        (NoArg
            (\opt -> return opt { optLexer = True }))
        "Only lex the files"
    ]

-- End Options

-- Prints a token stream or parser errors, depending on the verbose flag.
printTokenStream :: Bool -> [Token] -> IO ()
printTokenStream _ [] = putStr ""
printTokenStream verbose (t:ts) =
  do
    if verbose
    then
      putStrLn token_str
    else
      if isErrorToken t
      then
        putStrLn $ token_str
      else
        putStr ""
    printTokenStream verbose ts
    where (token_str) = printToken t

-- Builds the full token stream from a set of source files.
buildTokenStream :: ([FilePath], Bool) -> IO [Token]
buildTokenStream ([], _) = return []
buildTokenStream (fileNames, verbose) = do
  -- IO recursion
  tokenStream  <- makeTokenStream (head fileNames) verbose
  tokenStream' <- buildTokenStream (tail fileNames, verbose)
  -- Concatenate the TokenStreams
  return $ tokenStream ++ tokenStream'

-- Builds a single token stream from a source files.
makeTokenStream :: FilePath -> Bool -> IO [Token]
makeTokenStream file verbose =
  do source <- readFile file
     let tokenStream = Oodle.Lexer.lexer source file
     _ <- printTokenStream verbose tokenStream
     return tokenStream

-- Will eventually print a "pretty" parse tree as well
printParserOutput :: E a -> IO ()
printParserOutput (Ok _) = putStrLn "Parse OK"
printParserOutput (Failed a) = putStrLn a

-- Checks if the Parse is successful
verifyParse :: E a -> Bool
verifyParse (Ok _) = True
verifyParse (Failed _) = False


-- Counts Lexical Error Tokens
countErrorTokens :: [Token] -> Int
countErrorTokens tks = foldr (\x y -> if isErrorToken x then y + 1 else y) 0 tks

-- Main function
main :: IO ()
main = do
    -- Handle Options
    args <- getArgs
    prg <- getProgName
    let (actions, nonOptions, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optVerbose = verbose, optLexer = _ } = opts

    if length nonOptions < 1
    then
      do
        hPutStrLn stderr "FATAL ERROR: no input file(s)"
        hPutStrLn stderr (usageInfo prg options)
        exitWith $ ExitFailure 1
    else
      do
        -- Lex the input files
        tokenStream <- buildTokenStream (nonOptions, verbose)
        let errorCount = countErrorTokens tokenStream

        -- Parse the TokenStream
        let parseTree = Oodle.Parser.parser tokenStream
        let validTree = verifyParse parseTree

        if not validTree
        then
          printParserOutput parseTree
        else
          putStr ""

        if verbose
        then
          do
            putStrLn $ show (length tokenStream) ++ " Tokens found across " ++
              show (length nonOptions) ++ " file(s)."
            printParserOutput parseTree
        else putStr ""

        putStrLn $ show ((+) (if verifyParse parseTree then 0 else 1) errorCount) ++ " error(s) found"

        if not validTree || errorCount > 0
        then
          exitWith $ ExitFailure 1
        else
          exitSuccess
