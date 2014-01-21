import Oodle.Lexer

main = do
    putStrLn "Welcome to Luke Seelenbinder's Oodle Compiler"
    putStrLn "usage: oodle [-ds] <input file(s)>\n"
    getContents >>= print . Oodle.Lexer.lexer
