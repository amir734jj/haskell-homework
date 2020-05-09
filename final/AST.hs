{-# LANGUAGE FlexibleContexts #-}

module AST where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Writer

-- Abstract Syntax Tree

-- function/variable declaration
data Decl = Fun String String Exp -- fun f x = e;
          | Val String Exp        -- val x = e;

newtype DeclList = Decls [Decl]

-- expressions
data Exp = Lt Exp Exp     -- e1 < e2
         | Gt Exp Exp     -- e1 > e2
         | Eq Exp Exp     -- e1 = e2
         | Plus Exp Exp   -- e1 + e2
         | Minus Exp Exp  -- e1 - e2
         | Times Exp Exp  -- e1 * e2
         | Div Exp Exp    -- e1 div e2
         | Var String     -- x
         | If Exp Exp Exp -- if e0 then e1 else e2
         | Fn String Exp  -- fn x => e
         | Let DeclList Exp -- let val x = e0; fun f = e1; in e2 end
         | App Exp Exp    -- e1 e2
         | Const Integer  -- n

instance Show DeclList where
  show (Decls decls) = unlines $ map show decls

instance Show Decl where
  show (Fun f x e) = "fun " ++ f ++ " " ++ x ++ " = " ++ show e
  show (Val x e) = "val " ++ x ++ " = " ++ show e

instance Show Exp where
  show (Const x) = show x
  show (Plus e1 e2) = show_op e1 "+" e2
  show (Times e1 e2) = show_op e1 "*" e2
  show (Minus e1 e2) = show_op e1 "-" e2
  show (Div e1 e2) = show_op e1 "/" e2
  show (Lt e1 e2) = show_op e1 "<" e2
  show (Gt e1 e2) = show_op e1 ">" e2
  show (Eq e1 e2) = show_op e1 "=" e2
  show (If e0 e1 e2) = "if " ++ show e0 ++ " then " ++ show e1 ++ " else " ++ show e2
  show (Var s) = s
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Fn x e) = "(fn " ++ x ++ " => " ++ show e ++ ")"
  show (Let (Decls decls) e) = "(let " ++ show decls ++ " in " ++ show e ++ " end)"

show_op e1 op e2 = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"

-- reader+writer monad for pretty printing
--   reader for remembering indentation
--   writer for generating string
type PrettyPrint = ReaderT String (Writer String) ()

-- run the PrettyPrint monad (of a declaration list) to return a string
pp :: DeclList -> String
pp lst = snd $ runWriter $ runReaderT (ppl lst) ""

-- pretty print a list of declarations
ppl :: DeclList -> PrettyPrint
ppl (Decls decls) = mapM_ (\d -> ppd d >> tell "\n") decls

-- pretty print a declaration
ppd :: Decl -> PrettyPrint
ppd (Fun s1 s2 e1) = do
        -- the left part of the function declaration
        let leftPart = "fun " ++ s1 ++ " " ++ s2 ++ " = "
        -- we get the indentation
        indent <- ask
        -- we print the left part with the corresponding indentation
        tell $ indent ++ leftPart
        -- now we build the string to add to the new indentation
        let newIndent = take (length leftPart) (repeat ' ')
        -- then we do the pretty printing of the rest but adding the new indentation
        local (\s -> s ++ newIndent) (ppe e1)


ppd (Val s1 e1) = do
        -- the left part of the function declaration
        let leftPart = "val " ++ s1 ++ " = "
        -- we get the indentation
        indent <- ask
        -- we print the left part with the corresponding indentation
        tell $ indent ++ leftPart
        -- now we build the string to add to the new indentation
        let newIndent = take (length leftPart) (repeat ' ')
        -- then we do the pretty printing of the rest but adding the new indentation
        local (\s -> s ++ newIndent) (ppe e1)


-- pretty print an expression
ppe :: Exp -> PrettyPrint
ppe (If e1 e2 e3) = do
  -- we print the "if " word (no indentation needed)
  tell "if "
  -- we print the expression e1 immediately after the "if " word (no indentation needed)
  ppe e1
  -- we get the indentation
  indent <- ask
  -- print the "then " word in a new line with indentation
  tell $ "\n" ++ indent ++ "then "
  -- print the expression e1 immediately after the "then " word (no indentation needed)
  ppe e2
  -- print the "else " word in a new line with indentation
  tell $ "\n" ++ indent ++ "else "
  -- now we build the string to add to the new indentation
  let newIndent = take (length "else ") (repeat ' ')
  -- then we do the pretty printing of the rest but adding the new indentation
  local (\s -> s ++ newIndent) (ppe e3)

ppe (Fn s1 e1) = do
  -- the left part of the fn definition
  let leftPart = "fn " ++ s1 ++ " => "
  -- we print the left part (no indentation needed)
  tell leftPart
  -- now we build the string to add to the new indentation
  let newIndent = take (length leftPart) (repeat ' ')
  -- then we do the pretty printing of the rest but adding the new indentation
  local (\s -> s ++ newIndent) (ppe e1)

ppe (Let (Decls dl) e1) = do
  -- the word "let "
  let wLet = "let "
  -- we print the "let " word and finish the line (no indentation needed here)
  tell $ wLet ++ "\n"
  -- we get the indentation
  indent <- ask
  -- now we build the indentation for the "let" part
  let letIndent = take (length wLet) (repeat ' ')
  -- we print all the declarations passing the "let" indentation
  mapM_ (\d -> local (\s -> s ++ letIndent) (ppd d) >> tell "\n") dl
  --we print the word "in " with the corresponding indentation and finish the line
  tell $ indent ++ "in " ++ "\n"
  -- now we build the indentation for the "in" part
  let inIndent = take (length "in ") (repeat ' ')
  --we print the corresponding indentation
  tell $ indent ++ inIndent
  -- then we do the pretty printing of the expression but adding the "in" indentation
  local (\s -> s ++ inIndent) (ppe e1)
  -- we add a new line and print the "end" word with the corresponding indentation
  tell $ "\n" ++ indent ++ "end"

-- the remaining cases
ppe x = do
  tell $ show x
