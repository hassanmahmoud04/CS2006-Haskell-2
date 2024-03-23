module Expr where

import Parsing
import Text.Read (readMaybe)
import Data.Char (digitToInt)



type Name = String

data Value = IntVal Int | StrVal String | FloatVal Float
  deriving (Eq)

instance Show Value where
    show (IntVal a) = show a
    show (StrVal a) = show a
    show (FloatVal a) = show a

-- Expanded `Expr` to include variables and string literals (for ToString handling)
data Expr = Add Expr Expr
          | ToString Expr
          | ToInt Expr
          | ToFloat Expr
          | Val Value
          | Var Name           -- Added for variable support
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Abs Expr
          | Mod Expr Expr
          | Power Expr Expr
          | Stringconcat Expr Expr 
          | Input
          deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit 

  deriving Show



    
integerop :: (Int -> Int -> Int) -> Value -> Value -> Maybe Value
integerop f (IntVal x) (IntVal y) =  Just(IntVal(f x y))
integerop f _ _ = Nothing -- not two integer values

stringop ::(String -> String -> String) -> Value -> Value -> Maybe Value
stringop f (StrVal x) (StrVal y) =  Just(StrVal(f x y))
stringop f _ _ = Nothing


floatconv :: Int -> Int -> Float
floatconv x y =
    let intPart = fromIntegral x in
    let yLen = length (show y) in
    let decPart = fromIntegral y / (10.0 ^ yLen) in
    if x < 0
    then
        intPart - decPart
    else
        intPart + decPart


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
eval vars (Stringconcat x y) = do 
    a <- eval vars x
    b <- eval vars y
    stringop (++) a b
eval vars (Var n) = lookup n vars
eval vars (ToString x) = do
    a <- eval vars x
    case a of
        IntVal i -> return (StrVal (show i))
        StrVal s -> return (StrVal s)
        FloatVal s -> return (StrVal (show s))
eval vars (ToInt x) = do
    a <- eval vars x
    case a of
        StrVal s -> do
            n <- readMaybe s  
            return (IntVal n)
        IntVal i -> 
            return (IntVal i)
        FloatVal f -> 
            return (IntVal(round f))      
eval vars (Abs x) = do
    a <- eval vars x
    case a of
        IntVal i -> Just (IntVal (abs i))
        _ -> Nothing  -- or handle other types as needed
eval vars (Mod x y) = do
    a <- eval vars x
    b <- eval vars y
    case (a, b) of
        (IntVal i, IntVal j) -> Just (IntVal (mod i j))
        _ -> Nothing
eval vars (Power x y) = do
    a <- eval vars x
    b <- eval vars y
    case (a, b) of
        (IntVal i, IntVal j) -> Just (IntVal (i ^ j))
        _ -> Nothing
eval vars (ToFloat x) = do
    a <- eval vars x
    case a of
        StrVal s -> do
            n <- readMaybe s  
            return (FloatVal n)
        IntVal i -> 
            return (FloatVal (fromIntegral i))
        FloatVal f -> 
            return (FloatVal f)




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
           (do 
                spaces
                char '+'
                spaces
                e <- pExpr
                return (Add t e)
            ||| do 
                spaces 
                char '-'
                spaces 
                e <- pExpr
                return (Subtract t e))
            ||| do 
                spaces 
                string "++"
                spaces
                e <- pExpr
                return (Stringconcat t e)
            ||| return t


pFactor :: Parser Expr
pFactor = do 
                   d <- int
                   char '.'
                   i <- nat
                   return (Val (FloatVal(floatconv d i)))
             ||| do
                 d <- int
                 return (Val (IntVal(d)))
             ||| do 
                       string "input"
                       spaces 
                       return (Input)
                    
             ||| do 
                       string "toString"
                       spaces 
                       a <- pExpr
                       return (ToString a)
             ||| do 
                       string "toInt"
                       spaces 
                       a <- pExpr
                       return (ToInt a)
             ||| do 
                       string "toFloat"
                       spaces 
                       a <- pExpr
                       return (ToFloat a)
           ||| do v <- many1 letter
                  return (Var v)
           ||| do 
                       char '('
                       e <- pExpr
                       char ')'
                       return e
           ||| do 
                       char '"'
                       str <- many (alphanum ||| char '.')
                       char '"'
                       return (Val (StrVal(str)))
           ||| do 
                   char '|'
                   spaces
                   e <- pTerm
                   spaces 
                   char '|'
                   return (Abs e)


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
                   return (Divide f t)
            ||| do spaces
                   char '%'
                   spaces
                   t <- pTerm
                   return (Mod f t)
            ||| do spaces
                   char '^'
                   spaces
                   t <- pTerm
                   spaces
                   return (Power f t)
            ||| return f)


