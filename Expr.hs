module Expr where

import Data.Char (digitToInt)
import Control.Applicative ((<|>))

type Name = String

data Value = IntVal Int | FloatVal Float deriving (Show, Eq)

-- Extending Expr to include subtraction, multiplication, division, and support for Floats
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr
          | Neg Expr
          | ValInt Int
          | ValFloat Float
          | ToString Expr
          deriving Show

-- REPL commands remain unchanged
data Command = Set Name Expr
             | Print Expr
             deriving Show

-- The updated eval function
eval :: [(Name, Value)] -> Expr -> Either String Value
eval vars (ValInt x) = Right $ IntVal x
eval vars (ValFloat x) = Right (FloatVal x)
eval vars (Add x y) = binOp (+) (+) vars x y
eval vars (Sub x y) = binOp (-) (-) vars x y
eval vars (Mul x y) = binOp (*) (*) vars x y
eval vars (Div x y) = binOpF (/) vars x y
eval vars (Abs x) = absOp vars x
eval vars (Mod x y) = modOp vars x y
eval vars (Pow x y) = powOp vars x y
eval vars (Neg x) = negOp vars x
eval vars (ToString x) = Left "ToString operation not supported"

-- Helper functions for operations
binOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> [(Name, Value)] -> Expr -> Expr -> Either String Value
binOp fInt fFloat vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (IntVal xi, IntVal yi) -> Right $ IntVal (fInt xi yi)
        (FloatVal xf, FloatVal yf) -> Right $ FloatVal (fFloat xf yf)
        _ -> Left "Type mismatch in binary operation"

binOpF :: (Float -> Float -> Float) -> [(Name, Value)] -> Expr -> Expr -> Either String Value
binOpF f vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (FloatVal xf, FloatVal yf) -> if yf == 0 then Left "Division by zero error" else Right $ FloatVal (f xf yf)
        _ -> Left "Type mismatch in binary operation"

absOp :: [(Name, Value)] -> Expr -> Either String Value
absOp vars x = do
    xVal <- eval vars x
    case xVal of
        IntVal xi -> Right $ IntVal (abs xi)
        FloatVal xf -> Right $ FloatVal (abs xf)

modOp :: [(Name, Value)] -> Expr -> Expr -> Either String Value
modOp vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (IntVal xi, IntVal yi) -> if yi == 0 then Left "Modulus by zero error" else Right $ IntVal (xi `mod` yi)
        _ -> Left "Modulus operation requires integer operands"

powOp :: [(Name, Value)] -> Expr -> Expr -> Either String Value
powOp vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (FloatVal xf, FloatVal yf) -> Right $ FloatVal (xf ** yf)
        _ -> Left "Exponentiation type mismatch"

negOp :: [(Name, Value)] -> Expr -> Either String Value
negOp vars x = do
    xVal <- eval vars x
    case xVal of
        IntVal xi -> Right $ IntVal (negate xi)
        FloatVal xf -> Right $ FloatVal (negate xf)

-- The parser definitions remain unchanged, ensuring they are compatible with these operations
