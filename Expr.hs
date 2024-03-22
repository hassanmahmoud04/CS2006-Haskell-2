module Expr where

import Parsing
import Data.Maybe

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | ToString Expr
          | Val Value
          | Var Name
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit
  deriving Show

data Value = IntVal Int | StrVal [Char] 
  deriving (Show, Eq)

eval :: [(Name, Value)] -> Expr -> Maybe Value

eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = do
      a <- eval vars x
      b <- eval vars y
      return (a + b)
eval vars (Subtract x y) = do
    a <- eval vars x
    b <- eval vars y
    return (a - b)
eval vars (Multiply x y) = do
    a <- eval vars x
    b <- eval vars y
    return (a * b)
eval vars (Divide x y) = do
    a <- eval vars x
    b <- eval vars y
    if b == 0 then Nothing
              else return (a `div` b)
eval vars (Var n) = lookup n vars
-- eval vars (ToString x) = show x

-- digitToInt :: Value -> Int
-- digitToInt [x] = fromEnum x - fromEnum '0'
-- digitToInt (x:xs) = read (show (fromEnum x - fromEnum '0') ++ show (digitToInt xs)) :: Int
-- -- readDigit :: [Char] -> Int
-- -- readDigit 

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
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Subtract t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do
            t <- int
            return (Val $ IntVal t) 
           ||| do v <- many1 alphanum
                  return (Val $ StrVal v)
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Multiply f t)
            ||| do char '/'
                   t <- pTerm
                   return (Divide f t) 
                 ||| return f

-- toInt: read maybe or read either