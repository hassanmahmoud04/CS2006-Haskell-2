module Expr where

import Parsing

import Data.Char (digitToInt)


type Name = String

-- Expanded `Expr` to include variables and string literals (for ToString handling)
data Expr = Add Expr Expr
          | ToString Expr
          | Val Int
          | Var Name           -- Added for variable support
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
  deriving Show

eval :: [(Name, Int)] -> Expr -> Maybe Int
eval _ (Val x) = Just x
eval vars (Add x y) = do
    a <- eval vars x
    b <- eval vars y
    return (a + b)
eval vars (Subtract x y) = do  -- Assuming Subtract is part of Expr
    a <- eval vars x
    b <- eval vars y
    return (a - b)
eval vars (Multiply x y) = do  -- Assuming Multiply is part of Expr
    a <- eval vars x
    b <- eval vars y
    return (a * b)
eval vars (Divide x y) = do  -- Assuming Divide is part of Expr
    a <- eval vars x
    b <- eval vars y
    if b == 0 then Nothing  -- Guard against division by zero
              else return (a `div` b)
eval vars (Var n) = lookup n vars
eval vars (ToString x) = Nothing  -- Placeholder; requires different handling



-- The parser section remains unchanged but should be expanded to handle `Var` and potentially strings.
pCommand :: Parser Command
pCommand = do 
    var <- many1 letter -- Allow variable names longer than one character
    spaces               -- Optional spaces before '='
    char '='
    spaces               -- Optional spaces after '='
    expr <- pExpr
    return (Set var expr)
  ||| do 
    string "print"
    spaces               -- Optional spaces before expression
    expr <- pExpr
    return (Print expr)


pExpr :: Parser Expr
pExpr = do t <- pTerm
           (do char '+'
               e <- pExpr
               return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Subtract t e))
            ||| return t


pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
           ||| do v <- letter
                  -- Updated to support parsing variables
                  return (Var [v])
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           (do char '*'
               t <- pTerm
               return (Multiply f t)
            ||| do char '/'
                   t <- pTerm
                   -- Implement a check to prevent division by zero in the evaluation phase, not parsing
                   return (Divide f t))
            ||| return f
