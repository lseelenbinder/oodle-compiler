module Oodle.ScopedTreeWalker where

import Oodle.SymbolTable (SymbolTable,
  decl,
  Symbol,
  findSymbol,
  getMethods
  )
import Oodle.ParseTree
import Oodle.Token
import Oodle.Error

--                         Class   Method  Debug
type Scope = (SymbolTable, Symbol, Symbol, Bool)

class WalkableScoped a where

  walk            :: (WalkableScoped a) => SymbolTable -> Bool -> Start -> a
  walkClass       :: (WalkableScoped a) => Scope -> Class -> a
  walkMethod      :: (WalkableScoped a) => Scope -> Method -> a
  walkArgument    :: (WalkableScoped a) => Scope -> Argument -> a
  walkVariable    :: (WalkableScoped a) => Scope -> Var -> a
  walkStatement   :: (WalkableScoped a) => Scope -> Statement -> a
  walkExpression  :: (WalkableScoped a) => Scope -> Expression -> a

  -- dolass token name parentName variables methods
  doClass         :: (WalkableScoped a) => Scope -> Token -> Id -> Id -> a -> a -> a

  -- doMethod token name type arguments variables statments
  doMethod        :: (WalkableScoped a) => Scope -> Token -> Id -> Type -> a -> a -> a -> a

  -- doArgument token (IdArray name expressions) type
  doArgument      :: (WalkableScoped a) => Scope -> Token -> Id -> Type -> a

  -- doVariable token name type expression
  doVariable      :: (WalkableScoped a) => Scope -> Token -> Id -> Type -> Expression -> a

  -- doAssignStmt token (IdArray varName) expression
  doAssignStmt    :: (WalkableScoped a) => Scope -> Token -> Id -> Expression -> a

  -- doIfStmt token condition trueStatments falseStatments
  doIfStmt        :: (WalkableScoped a) => Scope ->
                      Token -> Expression -> a -> a -> a

  -- doLoopStmt token condition statments
  doLoopStmt      :: (WalkableScoped a) =>
                      Scope -> Token -> Expression -> a -> a

  -- doallStmt token scope methodName arguments
  doCallStmt      :: (WalkableScoped a) =>
                      Scope -> Token -> Expression -> Id -> [Expression] -> a
  -- doExpression  expression
  doExpression    :: (WalkableScoped a) => Scope -> Expression -> a

  reduce          :: (WalkableScoped a) => [a] -> a
  reduceMap       :: (WalkableScoped a) => (b -> a) -> [b] -> a
  reduceMap f list = reduce $ map f list

  walk st debug (Start classes) =
    reduceMap (walkClass (st, head st, head st, debug)) classes

  walkClass (st, _, _, debug) (Class tk (Id className) parentName vars methods) =
    reduce [doClass nScope tk (Id className) parentName v m, v, m]
    where c       = (\(Ok a) -> a) $ findSymbol st className
          nScope  = (st, c, c, debug)
          v       = reduceMap (walkVariable nScope) vars
          m       = reduceMap (walkMethod nScope) methods

  walkMethod (st, c, _, debug) (Method tk (Id name) typ arguments vars stmts) =
      doMethod nScope tk (Id name) typ a v s
    where m       = (\(Ok m') -> m') $ findSymbol (getMethods (decl c)) name
          nScope  = (st, c, m, debug)
          a       = reduceMap (walkArgument nScope) arguments
          v       = reduceMap (walkVariable nScope) vars
          s       = reduceMap (walkStatement nScope) stmts

  walkArgument scope (Argument tk (IdArray name exprs) typ) =
    reduce (doArgument scope tk (IdArray name exprs) typ : e)
    where e = map (walkExpression scope) exprs

  walkArgument scope (Argument tk (Id name) typ) =
    doArgument scope tk (Id name) typ

  walkVariable scope (Var tk name t expr) =
    reduce [doVariable scope tk name t expr, e]
    where e = walkExpression scope expr

  walkStatement scope s =
    let
      walkSParts = reduceMap (walkStatement scope)
    in
      case s of
        AssignStatement tk i expr ->
          doAssignStmt scope tk i expr
        IfStatement tk expr trueStmts falseStmts ->
          doIfStmt scope tk expr trueStmts' falseStmts'
          where trueStmts'  = walkSParts trueStmts
                falseStmts' = walkSParts falseStmts
        LoopStatement tk expr stmts ->
          doLoopStmt scope tk expr children
          where children  = walkSParts stmts
        CallStatement tk expr name arguments ->
          doCallStmt scope tk expr name arguments

  walkExpression = doExpression
