module Main (main) where

import Oodle.Lexer
import Oodle.Token
import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit

data Options = Options  { optVerbose :: Bool }

startOptions :: Options
startOptions = Options  { optVerbose = False }

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
              exitWith ExitSuccess)
              )
        "Show help"
    ]

printTokenStream :: Bool -> [TokenPosition] -> IO ()
printTokenStream _ [] = do putStr ""
printTokenStream verbose (t:ts) =
  do
    if verbose
    then
      putStrLn $ printToken t
    else
      if isErrorToken (getToken t)
      then
        putStrLn $ "Parsing Error: " ++ (show (Oodle.Token.getLine t)) ++ ": " ++ (show (getToken t))
      else
        putStr ""
    printTokenStream verbose ts


buildTokenStream :: ([FilePath], Bool) -> IO ([TokenPosition])
buildTokenStream ([], _) = return ([])
buildTokenStream (fileNames, verbose) = do
  -- IO recursion
  tokenStream  <- makeTokenStream (head fileNames) verbose
  tokenStream' <- buildTokenStream ((tail fileNames), verbose)
  -- Concatenate the TokenStreams
  return $ concat [tokenStream, tokenStream']

makeTokenStream :: FilePath -> Bool -> IO ([TokenPosition])
makeTokenStream file verbose =
  do source <- readFile file
     let tokenStream = Oodle.Lexer.lexer source file
     _ <- printTokenStream verbose tokenStream
     return (tokenStream)


main :: IO ()
main = do
    putStrLn "Welcome to Luke Seelenbinder's Oodle Compiler"

    -- Handle Options
    args <- getArgs
    prg <- getProgName
    let (actions, nonOptions, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optVerbose = verbose } = opts

    if not $ (length nonOptions) >= 1
    then
      do
        hPutStrLn stderr "FATAL ERROR: no input file(s)"
        hPutStrLn stderr (usageInfo prg options)
        exitWith $ ExitFailure 1
    else
      do
        -- Lex the input files
        tokenStream <- buildTokenStream (nonOptions, verbose)
        -- Parse the TokenStream
        if verbose
        then
          putStrLn $ (show (length tokenStream)) ++ " Tokens found across " ++ (show (length nonOptions)) ++ " files."
        else
          putStrLn "Done"
