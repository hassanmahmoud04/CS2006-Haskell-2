module Testing where

import Parsing
import Expr
import REPL

import Test.QuickCheck
-- import Test.QuickCheckAll

prop_add_int :: Int -> Int -> Bool
prop_add_int x y = eval mempty (Add (Val (IntVal x)) (Val (IntVal y))) == Just (IntVal (x + y))

prop_add_float :: Float -> Float -> Bool
prop_add_float x y = eval mempty (Add (Val (FloatVal x)) (Val (FloatVal y))) == Just (FloatVal (x + y))

prop_subtract_int :: Int -> Int -> Bool
prop_subtract_int x y = eval mempty (Subtract (Val (IntVal x)) (Val (IntVal y))) == Just (IntVal (x - y))

prop_subtract_float :: Float -> Float -> Bool
prop_subtract_float x y = eval mempty (Subtract (Val (FloatVal x)) (Val (FloatVal y))) == Just (FloatVal (x - y))

prop_multiply_int :: Int -> Int -> Bool
prop_multiply_int x y = eval mempty (Multiply (Val (IntVal x)) (Val (IntVal y))) == Just (IntVal (x * y))

prop_multiply_float :: Float -> Float -> Bool
prop_multiply_float x y = eval mempty (Multiply (Val (FloatVal x)) (Val (FloatVal y))) == Just (FloatVal (x * y))

prop_divide_int :: NonZero Int -> NonZero Int -> Bool
prop_divide_int (NonZero x) (NonZero y) = eval mempty (Divide (Val (IntVal x)) (Val (IntVal y))) == Just (IntVal (div x y))

prop_divide_float :: NonZero Float -> NonZero Float -> Bool
prop_divide_float (NonZero x) (NonZero y) = eval mempty (Divide (Val (FloatVal x)) (Val (FloatVal y))) == Just (FloatVal (x / y))