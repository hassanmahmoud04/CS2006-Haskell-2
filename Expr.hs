module Expr where

import Parsing
import Data.Char (digitToInt)
import Control.Applicative ((<|>))

type Name = String

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

-- Updated eval to handle new operations and Floats. Mixed types (Int and Float) are coerced to Float.
eval :: [(Name, Int)] -> Expr -> Maybe Float
eval vars (ValInt x) = Just $ fromIntegral x
eval vars (ValFloat x) = Just x
eval vars (Add x y) = (+) <$> eval vars x <*> eval vars y
eval vars (Sub x y) = (-) <$> eval vars x <*> eval vars y
eval vars (Mul x y) = (*) <$> eval vars x <*> eval vars y
eval vars (Div x y) = (/) <$> eval vars x <*> eval vars y
eval vars (Abs x) = abs <$> eval vars x
eval vars (Mod x y) = fromIntegral . mod <$> (fmap round $ eval vars x) <*> (fmap round $ eval vars y)
eval vars (Pow x y) = (**) <$> eval vars x <*> eval vars y
eval vars (Neg x) = negate <$> eval vars x
eval vars (ToString x) = Nothing  -- Handling for ToString needs to be implemented based on requirements

-- Parser updated to handle floating-point numbers and negative literals
pExpr :: Parser Expr
pExpr = pTerm `chainl1` addop
  where addop = do{ char '+'; return Add }
               <|> do{ char '-'; return Sub }

pTerm :: Parser Expr
pTerm = pFactor `chainl1` mulop
  where mulop = do{ char '*'; return Mul }
               <|> do{ char '/'; return Div }

pFactor :: Parser Expr
pFactor = pFloat <|> pInt <|> pNeg <|> pParens pExpr
  where pInt = ValInt . read <$> many1 digit
        pFloat = ValFloat . read <$> (do
          intPart <- many1 digit
          char '.'
          fracPart <- many1 digit
          return $ intPart ++ "." ++ fracPart)
        pNeg = char '-' >> Neg <$> pFactor
        pParens p = do
          char '('
          e <- p
          char ')'
          return e

-- Parsing.hs would need adjustments for complete integration, especially for operations like Abs, Mod, Pow
