module Expr where

import Data.Char (digitToInt)
import Control.Applicative ((<|>))
import Data.Fixed (mod')


type Name = String

data BST k v = Empty | Node k v (BST k v) (BST k v) deriving Show

type VarTree = BST String Value

insert :: Ord k => k -> v -> BST k v -> BST k v
insert key val Empty = Node key val Empty Empty
insert key val (Node k v left right)
    | key < k = Node k v (insert key val left) right
    | key > k = Node k v left (insert key val right)
    | otherwise = Node key val left right  -- Overwrite existing

lookupVar :: Ord k => k -> BST k v -> Maybe v
lookupVar key Empty = Nothing
lookupVar key (Node k v left right)
    | key < k = lookupVar key left
    | key > k = lookupVar key right
    | otherwise = Just v

data Value = IntVal Int | FloatVal Float deriving (Show, Eq)

data Expr
    = Add Expr Expr
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
    | Var Name
    | If Expr Expr Expr
    deriving Show

data Command
    = Set Name Expr
    | ReadFile FilePath
    | Print Expr
    deriving Show

eval :: VarTree -> Expr -> Either String Value
eval _ (ValInt x) = Right $ IntVal x
eval _ (ValFloat x) = Right $ FloatVal x
eval vars (Var name) = maybe (Left $ "Variable not found: " ++ name) Right (lookupVar name vars)
eval vars (Add x y) = binaryOperation (+) vars x y
eval vars (Sub x y) = binaryOperation (-) vars x y
eval vars (Mul x y) = binaryOperation (*) vars x y
eval vars (Div x y) = divideOperation vars x y
eval vars (Abs x) = absOperation vars x
eval vars (Mod x y) = modOperation vars x y
eval vars (Pow x y) = powOperation vars x y
eval vars (Neg x) = negOperation vars x
eval vars (ToString x) = Left "ToString operation not supported"
eval vars (If cond thenBranch elseBranch) = ifOperation vars cond thenBranch elseBranch

binaryOperation :: (Float -> Float -> Float) -> VarTree -> Expr -> Expr -> Either String Value
binaryOperation op vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (FloatVal xf, FloatVal yf) -> Right $ FloatVal (op xf yf)
        _ -> Left "Type mismatch in binary operation"

divideOperation :: VarTree -> Expr -> Expr -> Either String Value
divideOperation vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (FloatVal xf, FloatVal yf) ->
            if yf == 0 then Left "Division by zero error"
            else Right $ FloatVal (xf / yf)
        _ -> Left "Type mismatch in division operation"

absOperation :: VarTree -> Expr -> Either String Value
absOperation vars x = do
    xVal <- eval vars x
    case xVal of
        FloatVal xf -> Right $ FloatVal (abs xf)
        _ -> Left "Unsupported type for abs"

modOperation :: VarTree -> Expr -> Expr -> Either String Value
modOperation vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        -- For integers, use `mod`. Ensure types align if you use `mod` directly.
        (IntVal xi, IntVal yi) -> if yi == 0 then Left "Modulus by zero error" else Right $ IntVal (xi `mod` yi)
        -- For floating points, use `mod'` from Data.Fixed if appropriate.
        (FloatVal xf, FloatVal yf) -> Right $ FloatVal (xf `mod'` yf)
        _ -> Left "Modulus operation requires numeric operands"


powOperation :: VarTree -> Expr -> Expr -> Either String Value
powOperation vars x y = do
    xVal <- eval vars x
    yVal <- eval vars y
    case (xVal, yVal) of
        (FloatVal xf, FloatVal yf) -> Right $ FloatVal (xf ** yf)
        _ -> Left "Exponentiation type mismatch"

negOperation :: VarTree -> Expr -> Either String Value
negOperation vars x = do
    xVal <- eval vars x
    case xVal of
        FloatVal xf -> Right $ FloatVal (negate xf)
        _ -> Left "Unsupported type for negation"

ifOperation :: VarTree -> Expr -> Expr -> Expr -> Either String Value
ifOperation vars cond thenBranch elseBranch = do
    condVal <- eval vars cond
    case condVal of
        FloatVal n -> if n /= 0.0 then eval vars thenBranch else eval vars elseBranch
        _ -> Left "Condition in If must be a non-zero value for the then branch"
