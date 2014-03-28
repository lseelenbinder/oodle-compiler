-- Filename: Main.hs
-- Contents: The framework for running the compiler. Parses options and runs
-- the compiler sequence.

module Main (main) where

import Oodle.Parser
import Oodle.Lexer
import Oodle.Token
import Oodle.Error
import Oodle.UnsupportedFeatures (unsupportedFeatures)
import Oodle.SymbolTable (symbolTableBuilder)
import Oodle.TypeChecker (typeChecker)
import Oodle.CodeGenerator (codeGenerator)
import System.Console.GetOpt
import System.IO
import System.Directory (removeFile)
import System.Environment
import System.Exit
import System.Process (readProcess)
import Control.Monad

-- The following functions are related to options handling and mostly copied from
-- http://hackage.haskell.org/package/base-4.6.0.1/docs/System-Console-GetOpt.html
data Options = Options  { optVerbose :: Bool, optLexer :: Bool,
                          optAssembly :: Bool, optDebug :: Bool }

startOptions :: Options
startOptions = Options  { optVerbose = False, optLexer = False,
                          optAssembly = False, optDebug = False }

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
    , Option "S" []
        (NoArg
            (\opt -> return opt { optAssembly = True }))
        "Only produce assembly code; do not produce an executable"
    , Option "g" []
        (NoArg
            (\opt -> return opt { optDebug = True }))
        "Enable debugging"
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
printParserOutput :: Error a -> String
printParserOutput (Ok _) = "Parse OK"
printParserOutput (Failed msg) = msg

-- Checks if the Parse is successful
verifyParse :: Error a -> Bool
verifyParse (Ok _) = True
verifyParse (Failed _) = False

-- Counts Lexical Error Tokens
countErrorTokens :: [Token] -> Int
countErrorTokens = foldr (\x y -> if isErrorToken x then y + 1 else y) 0

-- Filters invalid tokens
filterInvalidTokens :: [Token] -> [Token]
filterInvalidTokens = filter (not . isErrorToken)

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
    let Options { optVerbose = verbose, optLexer = lexerOnly,
                  optAssembly = assemblyOnly, optDebug = debug } = opts

    if null nonOptions
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
          hPutStrLn stderr $ printErrorCount (succ errorCount)
          exitWith $ ExitFailure 1

        let parseTree' = deE parseTree

        when verbose $
          hPrint stderr parseTree'

        -- Run Semantic Checks
        --
        -- Unsupported Features
        let unsupportedFeatures' = unsupportedFeatures parseTree'
        let warningCount = length unsupportedFeatures'
        unless (warningCount == 0) $
          hPutStrLn stderr $ tail $
            concatMap (\s -> "\nUnsupported Feature: " ++ s) unsupportedFeatures'

        -- Symbol Table
        let symbolTable = symbolTableBuilder parseTree'
        let validST = verifyParse symbolTable
        unless validST $ do
          hPutStrLn stderr $ printParserOutput symbolTable
          hPutStrLn stderr $ printErrorCount (succ errorCount)
          exitWith $ ExitFailure 1

        let symbolTable' = deE symbolTable
        when ((length symbolTable' - 4) > length nonOptions) $
          hPutStrLn stderr "Error: more than one class per file"

        -- Type Checking
        let tc = typeChecker symbolTable' parseTree'

        unless (verifyParse tc) $ do
          hPutStrLn stderr $ printParserOutput tc
          hPutStrLn stderr $ printErrorCount (succ errorCount)
          exitWith $ ExitFailure 1

        when (warningCount > 0) $ do
          hPutStrLn stderr $ printWarningCount warningCount
          hPutStrLn stderr
            "Cannot produce code for programs with unsupported features."
          exitWith $ ExitFailure 1

        -- Code Generation
        let assembly = codeGenerator symbolTable' debug parseTree'
        let outName = takeWhile (/= '.') (last nonOptions)

        if assemblyOnly then do
          out <- openFile (outName ++ ".s") WriteMode
          hPutStrLn out assembly
          hClose out
        else do
          tmp <- openFile "/tmp/file.s" WriteMode
          hPutStrLn tmp assembly
          hClose tmp
          readProcess "gcc" [
              "-o" ++ outName,
              "stdlib.o",
              "/tmp/file.s"
              ] ""
          removeFile "/tmp/file.s"

        exitSuccess
