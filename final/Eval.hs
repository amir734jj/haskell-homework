{-# LANGUAGE FlexibleContexts #-}

module Eval where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Writer
import Control.Monad.Except

import AST

type Context = [(String, Val)]

data Val = IntVal Integer
         | BoolVal Bool
         | FVal (Maybe String, String, Exp) Context

instance Show Val where
  show (IntVal x) = show x
  show (BoolVal b) = show b
  show (FVal (Just f, x, e) _) =  "fn" -- "fun " ++ f ++ "..."
  show (FVal (Nothing, x, e) _) =  "(fn " ++ x ++ " => ...)"

data EvalError = VariableNotFound String
               | NotAnInt Val
               | NotABool Val
               | DivByZero
               | NotAFun Val deriving (Show)

-- reader+either monad for interpreter functions
--   'reader' for remembering val/fun declarations in contexts
--   'either' for throwing evaluation errors
type Eval = ReaderT Context (Either EvalError)


-- auxiliary function that evaluates the given binary boolean operation of two Val
evalBinBool :: (Integer -> Integer -> Bool) -> Val -> Val -> Eval Val
-- If Val arguments are integers then we return the result of the boolean operation
evalBinBool op (IntVal n1) (IntVal n2) = return $ (BoolVal (op n1 n2))
-- The other cases we report the wrong argument
evalBinBool op (IntVal n1) v2 = lift $ Left (NotAnInt v2)
evalBinBool op v1 v2 = lift $ Left (NotAnInt v1)

-- auxiliary function that evaluates the given binary integer operation of two Val
evalBinInt :: (Integer -> Integer -> Integer) -> Val -> Val -> Eval Val
-- If Val arguments are integers then we return the result of the integer operation
evalBinInt op (IntVal n1) (IntVal n2) = return $ (IntVal (op n1 n2))
-- The other cases we report the wrong argument
evalBinInt op (IntVal n1) v2 = lift $ Left (NotAnInt v2)
evalBinInt op v1 v2 = lift $ Left (NotAnInt v1)

-- auxiliary function to create the "Eval Context" function (ReaderT instance) associated to a declaration
declContext :: Decl -> Eval Context
declContext d = do
      -- we get the context
      context <- ask
      -- then put the corresponding context the the front of the 'context' list
      case d of (Fun s1 s2 e1) -> return $ (s1, FVal ((Just s1), s2, e1) context):context
                (Val s1 e1) -> do
                                  -- for a value we need to calculate the value of e1
                                  res <- eval e1
                                  -- then assign that value to s1
                                  return $ (s1, res):context

-- evaluate a list of declarations
evalD :: DeclList -> Eval Context
-- in this function we make the composition of the corresponding "Eval Context" for each declaration on the list
evalD (Decls dList) =  do
      -- first we get the corresponding function for each declaration in the list using declContext function above
      let array = map (runReaderT.declContext) dList
      -- then we compose all those functions (Kleisli composition)
      let result = foldl1 (>=>) array
      -- we get the context
      context <- ask
      -- then get the result according to the composition stored in "result" and return the corresponding result
      case (result context) of (Left x) -> lift $ Left x
                               (Right x) -> return x

-- evaluate expression
eval :: Exp -> Eval Val
-- for constant we just return the corresponding constant wrapped in an IntVal
eval (Const n) = do
    return $ (IntVal n)
-- for Bool valued functions we evaluate the result using the auxiliary function evalBinBool
eval (Lt e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinBool (<) v1 v2
-- for Bool valued functions we evaluate the result using the auxiliary function evalBinBool
eval (Gt e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinBool (>) v1 v2
-- for Bool valued functions we evaluate the result using the auxiliary function evalBinBool
eval (Eq e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinBool (==) v1 v2
-- for Integer valued functions we evaluate the result using the auxiliary function evalBinInt
eval (Plus e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinInt (+) v1 v2
-- for Integer valued functions we evaluate the result using the auxiliary function evalBinInt
eval (Minus e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinInt (-) v1 v2
-- for Integer valued functions we evaluate the result using the auxiliary function evalBinInt
eval (Times e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinInt (*) v1 v2
-- for Integer valued functions we evaluate the result using the auxiliary function evalBinInt
eval (Div e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    evalBinInt (div) v1 v2
-- the Var case
eval (Var s1) = do
    -- we get the context
    context <- ask
    -- then look for the value in "context"
    let value = lookup s1 context
    -- check the result of the search
    case value of (Just x) -> return x  -- if found we return the value
                  Nothing -> lift $ Left (VariableNotFound s1)  -- if not found then report error
-- the If case
eval (If e1 e2 e3) = do
    -- we get the context
    context <- ask
    -- evaluate the result of e1 (implicitly with the context "context")
    res <- eval e1
    -- check if the value of e1 is a boolean value
    case res of (BoolVal b) -> if b then (eval e2) else (eval e3)  -- if so, return the evaluation of the corresponding expression (implicitly with the context "context")
                otherwise -> lift $ Left (NotABool res)   -- if not, report that the value "res" of "e1" is not Bool
-- the Fn case
eval (Fn s1 e1) = do
    -- we get the context
    context <- ask
    -- return the corresponding designated Val value for Fn attaching the context "context"
    return $ FVal (Nothing, s1, e1) context
-- the Let case
eval (Let declList e1) = do
    -- we create the new declaration context after all the declarations in declList have been added to the empty list []
    let newDeclContext = runReaderT (evalD declList) []
    -- we check for errors on those declarations
    case newDeclContext of (Right newContext) -> local (\c -> newContext ++ c) (eval e1)     -- if no error, then we behave as eval e1 but to every context we add "newContext" at the front
                           (Left x) -> lift $ Left x   -- if there is an error, then report it
-- the App case
eval (App e1 e2) = do
    -- we get the result of the parameter e1 that is passed to the function e1
    res <- eval e2
    -- we apply the function depending on thw shape of e1
    case e1 of (Var s1) -> do
                              -- the (Var s1) case is when e1 is already a function of the form Fun String String Exp, that is, a function with a name "s1" below
                              -- we get the context
                              context <- ask
                              -- we search the context for the function with name in "s1" in the context "context"
                              let r1 = lookup s1 context
                              -- we check if there is such declaration
                              case r1 of Nothing -> lift $ Left (VariableNotFound s1)  -- if not, then we report the error
                                         (Just y) -> do
                                                        -- we check that the found value corresponds to a function
                                                        case y of (FVal (_,s11,e11) fContext) -> local (\c -> ((s11, res):fContext) ++ c) (eval e11)  -- if a function, then we behave as the expression e11 that defines "s1" but adding the definition context "fContext" to the function along with the value "res" that "s11" has to take to evaluate the function
                                                                  otherwise -> lift $ Left (NotAFun y)   -- if not a function, then we report
               -- for other cases then e1 is the result of a function evaluation and we proceed as such
               otherwise -> do
                              -- we evaluate e1
                              r2 <- eval e1
                              -- then check if it is a function  and proceed to do the calculation (similar to few lines above)
                              case r2 of (FVal (_,s11,e11) fContext) -> local (\c -> ((s11, res):fContext) ++ c) (eval e11)
                                         otherwise -> lift $ Left (NotAFun r2)


-- run a list of declarations and print the resulting context
runD :: DeclList -> String
runD d = y
  where Right y = runReaderT x []
        x = do
              a <- evalD d
              return $ "answers:\n" ++ toString a ++ "\n"
            `catchError` (\e -> return $ show e ++ "\n")
        toString a = unlines $ map (\(x,v)-> "\t val " ++ x ++ " = " ++ show v) $ reverse a

-- run an expression and print the results
runE :: Exp -> String
runE e = y
  where Right y = runReaderT x []
        x = do
              a <- eval e
              return $ "answers: " ++ show a ++ "\n"
            `catchError` (\e -> return $ show e ++ "\n")
