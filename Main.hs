-- Filename: Main.hs
-- Contents: The framework for running the compiler. Parses options and runs
-- the compiler sequence.

module Main (main) where

import Oodle.Parser
import Oodle.Lexer
import Oodle.Token
import Oodle.TreeWalker
import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Data.List (intercalate)

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
printTokenStream :: Bool -> [Token] -> String
printTokenStream _ [] = ""
printTokenStream verbose (t:ts) =
  (if verbose || isErrorToken t then printToken t ++ "\n" else "") ++
    printTokenStream verbose ts

-- Builds the full token stream from a set of source files.
buildTokenStream :: [FilePath] -> IO [Token]
buildTokenStream [] = return []
buildTokenStream fileNames = do
  -- IO recursion
  tokenStream  <- makeTokenStream (head fileNames)
  tokenStream' <- buildTokenStream (tail fileNames)
  -- Concatenate the TokenStreams
  return $ tokenStream ++ tokenStream'

-- Builds a single token stream from a source files.
makeTokenStream :: FilePath -> IO [Token]
makeTokenStream file =
  do source <- readFile file
     return $ Oodle.Lexer.lexer source file

-- Will eventually print a "pretty" parse tree as well
printParserOutput :: E a -> String
printParserOutput (Ok _) = "Parse OK"
printParserOutput (Failed msg) = msg
printParserOutput (SemanticFail msg) = msg

-- Checks if the Parse is successful
verifyParse :: E a -> Bool
verifyParse (Ok _) = True
verifyParse _ = False

-- Counts Lexical Error Tokens
countErrorTokens :: [Token] -> Int
countErrorTokens = foldr (\x y -> if isErrorToken x then y + 1 else y) 0

-- Filters invalid tokens
filterInvalidTokens :: [Token] -> [Token]
filterInvalidTokens = filter (not . isErrorToken)

deE :: E a -> a
deE (Ok a) = a

printErrorCount :: Int -> String
printErrorCount c =
  show c ++ " error(s) found"

printWarningCount :: Int -> String
printWarningCount c =
  show c ++ " warning(s) found"

-- Main function
main :: IO ()
main = do
    -- Handle Options
    args <- getArgs
    prg <- getProgName
    let (actions, nonOptions, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optVerbose = verbose, optLexer = lexerOnly } = opts

    if length nonOptions < 1
    then
      do
        hPutStrLn stderr "FATAL ERROR: no input file(s)"
        hPutStrLn stderr (usageInfo prg options)
        exitWith $ ExitFailure 1
    else
      do
        -- Lex the input files
        tokenStream <- buildTokenStream nonOptions
        let errorCount = countErrorTokens tokenStream
        when (errorCount > 0) $
          hPutStrLn stderr $ printTokenStream verbose tokenStream

        when (verbose || lexerOnly ) $
          hPutStrLn stderr $ show (length tokenStream) ++ " Token(s) found across " ++
            show (length nonOptions) ++ " file(s)."

        when lexerOnly $
          exitWith (if errorCount == 0 then ExitFailure 1 else ExitSuccess)

        -- Parse the token stream
        let parseTree = Oodle.Parser.parser (filterInvalidTokens tokenStream)
        let validParse = verifyParse parseTree

        unless validParse $ do
          hPutStrLn stderr $ printParserOutput parseTree
          hPutStrLn stderr $ printErrorCount (errorCount + 1)
          exitWith $ ExitFailure 1

        when verbose $
          hPrint stderr parseTree

        -- Run Semantic Checks
        --
        -- Unsupported Features
        let unsupportedFeatures' = unsupportedFeatures $ deE parseTree
        let warningCount = length unsupportedFeatures'
        hPutStrLn stderr $ intercalate "\n" unsupportedFeatures'

        -- Symbol Table
        let symbolTable = symbolTableBuilder (deE parseTree)
        -- force SymbolTable to fully evaluate
        writeFile "/dev/null" (show symbolTable)

        when ((length symbolTable - 4) > length nonOptions) $
          hPutStrLn stderr "Error: more than one class per file"

        -- Type Checking
        let tc = typeChecker symbolTable (deE parseTree)
        let tcErrorCount = if tc then errorCount else errorCount + 1

        if tcErrorCount > 0
        then do
          hPutStrLn stderr $ printErrorCount errorCount
          exitFailure
        else do
          hPutStrLn stderr $ printWarningCount warningCount
          putStrLn "Compilation Successful"
          exitSuccess
