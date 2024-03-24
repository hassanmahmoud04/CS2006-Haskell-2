module Testing where

import Parsing
import Expr
import REPL

import Test.QuickCheck
-- import Test.QuickCheckAll

--test cases for addition operation
prop_add_int :: Int -> Int -> Bool
prop_add_int x y = eval mempty (Add (Val $ IntVal x) (Val $ IntVal y)) == Just (IntVal $ x + y)

prop_add_float :: Float -> Float -> Bool
prop_add_float x y = eval mempty (Add (Val $ FloatVal x) (Val $ FloatVal y)) == Just (FloatVal $ x + y)

--test cases for subtraction operation
prop_subtract_int :: Int -> Int -> Bool
prop_subtract_int x y = eval mempty (Subtract (Val $ IntVal x) (Val $ IntVal y)) == Just (IntVal $ x - y)

prop_subtract_float :: Float -> Float -> Bool
prop_subtract_float x y = eval mempty (Subtract (Val $ FloatVal x) (Val $ FloatVal y)) == Just (FloatVal $ x - y)

--test cases for multiplication operation
prop_multiply_int :: Int -> Int -> Bool
prop_multiply_int x y = eval mempty (Multiply (Val $ IntVal x) (Val $ IntVal y)) == Just (IntVal $ x * y)

prop_multiply_float :: Float -> Float -> Bool
prop_multiply_float x y = eval mempty (Multiply (Val $ FloatVal x) (Val $ FloatVal y)) == Just (FloatVal $ x * y)

--test cases for division operation
prop_divide_int :: NonZero Int -> NonZero Int -> Bool
prop_divide_int (NonZero x) (NonZero y) = eval mempty (Divide (Val $ IntVal x) (Val $ IntVal y)) == Just (IntVal $ div x y)

prop_divide_float :: NonZero Float -> NonZero Float -> Bool
prop_divide_float (NonZero x) (NonZero y) = eval mempty (Divide (Val $ FloatVal x) (Val $ FloatVal y)) == Just (FloatVal $ x / y)

--test cases for string concatenation operation
prop_string_concat :: String -> String -> Bool
prop_string_concat x y = eval mempty (Stringconcat (Val $ StrVal x) (Val $ StrVal y)) == Just (StrVal $ x ++ y)

--test cases for toString function
prop_toString_int :: Int -> Bool
prop_toString_int x = eval mempty (ToString $ Val $ IntVal x) == Just (StrVal $ show x)

prop_toString_float :: Float -> Bool
prop_toString_float x = eval mempty (ToString $ Val $ FloatVal x) == Just (StrVal $ show x)

prop_toString_string :: String -> Bool
prop_toString_string x = eval mempty (ToString $ Val $ StrVal x) == Just (StrVal x)

--test cases for absolute value function
prop_abs_int :: Int -> Bool
prop_abs_int x = eval mempty (Abs $ Val $ IntVal x) == Just (IntVal $ abs x)

--test cases for modulus function
prop_mod_int :: NonZero Int -> NonZero Int -> Bool
prop_mod_int (NonZero x) (NonZero y) = eval mempty (Mod (Val $ IntVal x) (Val $ IntVal y)) == Just (IntVal $ mod x y)

--test cases for power function
prop_power_int :: Int -> NonNegative Int -> Bool
prop_power_int x (NonNegative y) = eval mempty (Power (Val $ IntVal x) (Val $ IntVal y)) == Just (IntVal $ x ^ y)

prop_power_float :: Float -> NonNegative Float -> Bool
prop_power_float x (NonNegative y) = eval mempty (Power (Val $ FloatVal x) (Val $ FloatVal y)) == Just (FloatVal $ x ** y)

--test cases for toFloat function
prop_toFloat_int :: Int -> Bool
prop_toFloat_int x = eval mempty (ToFloat $ Val $ IntVal x) == Just (FloatVal $ fromIntegral x)

prop_toFloat_float :: Float -> Bool
prop_toFloat_float x = eval mempty (ToFloat $ Val $ FloatVal x) == Just (FloatVal x)

--test cases for negation operation
prop_neg_int :: Int -> Bool
prop_neg_int x = eval mempty (Neg $ Val $ IntVal x) == Just (IntVal $ -x)