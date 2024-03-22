module Expr where

import Parsing

import Data.Char (digitToInt)


type Name = String

-- Expanded `Expr` to include variables and string literals (for ToString handling)
data Expr = Add Expr Expr
          | ToString Expr
          | Val Value
          | Var Name           -- Added for variable support
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Stringliteral String  
          | Stringconcat Expr Expr 
          deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit 

  deriving Show

data Value = IntVal Int | StrVal String
  deriving (Show,Eq)
    
integerop :: (Int -> Int -> Int) -> Value -> Value -> Maybe Value
integerop f (IntVal x) (IntVal y) =  Just(IntVal(f x y))
integerop f _ _ = Nothing -- not two integer values


eval :: [(Name, Value)] -> Expr -> Maybe Value
eval _ (Val x) = Just x
eval vars (Add x y) = do
    a <- eval vars x
    b <- eval vars y
    integerop (+) a b 
eval vars (Subtract x y) = do  -- Assuming Subtract is part of Expr
    a <- eval vars x
    b <- eval vars y
    integerop (-) a b 
eval vars (Multiply x y) = do  -- Assuming Multiply is part of Expr
    a <- eval vars x
    b <- eval vars y
    integerop (*) a b 
eval vars (Divide x y) = do  -- Assuming Divide is part of Expr
    a <- eval vars x
    b <- eval vars y
    if b == IntVal(0) then Nothing  -- Guard against division by zero
        else integerop (div) a b 
eval vars (Var n) = lookup n vars
eval vars (ToString x) = Nothing  -- Placeholder; requires different handling



-- The parser section remains unchanged but should be expanded to handle `Var` and potentially strings.
pCommand :: Parser Command
pCommand = do t <- many1 letter
              space
              char '='
              space
              e <- pExpr
              return (Set t e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
                 ||| do string "quit"
                        return Quit


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
pFactor = do d <- nat
             return (Val (IntVal(d)))
           ||| do v <- many1 letter
                  return (Var v)
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           (do spaces 
               char '*'
               spaces
               t <- pTerm
               return (Multiply f t)
            ||| do spaces
                   char '/'
                   spaces
                   t <- pTerm
                   -- Implement a check to prevent division by zero in the evaluation phase, not parsing
                   return (Divide f t))
            ||| return f
