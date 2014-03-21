module Oodle.Error where

import Oodle.Token (Token, printToken)

data Error a = Ok a | Failed String
  deriving Show

deE :: Error a -> a
deE (Ok a) = a

instance Monad Error where
  (>>=) m k =
    case m of
      Ok a -> k a
      Failed e -> fail e
  return = Ok
  fail = Failed

msgWithToken' :: Token -> String -> String
msgWithToken' tk msg = msgWithToken tk msg ""

msgWithToken :: Token -> String -> String -> String
msgWithToken tk msg name = printToken tk ++ name' ++ ": " ++ msg
  where name' = if name /= "" then ": " ++ name else name

