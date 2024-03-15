module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import Expr (Command(..), Expr(..)) -- Assuming Expr and Command types are defined here

infixr 5 |||

pIf :: Parser Expr
pIf = do
    _ <- string "if"
    _ <- space
    cond <- pExpr
    _ <- space
    _ <- string "then"
    _ <- space
    thenExpr <- pExpr
    _ <- space
    _ <- string "else"
    _ <- space
    elseExpr <- pExpr
    return $ If cond thenExpr elseExpr


newtype Parser a = P (String -> [(a, String)])

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do { a <- p; rest a }
  where
    rest a = (do f <- op
                 b <- p
                 rest (f a b))
             <|> return a

instance Functor Parser where
   fmap f p = P (\inp -> case parse p inp of
                          [] -> []
                          result -> map (\(v, out) -> (f v, out)) result)

instance Applicative Parser where
   pure v = P (\inp -> [(v, inp)])
   f <*> a = P (\inp -> case parse f inp of
                         [] -> []
                         result -> concatMap (\(g, out) -> parse (fmap g a) out) result)

instance Monad Parser where
   return = pure
   p >>= f = P (\inp -> concatMap (\(v, out) -> parse (f v) out) (parse p inp))

instance Alternative Parser where
   empty = P (const [])
   p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         res -> res)

instance MonadPlus Parser where
   mzero = empty
   mplus = (<|>)

failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\inp -> case inp of
                   [] -> []
                   (x:xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(|||) :: Parser a -> Parser a -> Parser a
p ||| q = p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p ||| return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       ||| nat

float :: Parser Float
float = do whole <- many1 digit
           char '.'
           fractional <- many1 digit
           return $ read (whole ++ "." ++ fractional)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- New parser for commands
pCommand :: Parser Command
pCommand = pSet <|> pPrint
  where
    pSet = do
        string "set"
        space
        name <- identifier
        space
        char '='
        space
        expr <- pExpr -- Assuming you have a function pExpr for parsing expressions
        return (Set name expr)
    pPrint = do
        string "print"
        space
        expr <- pExpr
        return (Print expr)

pExpr :: Parser Expr
pExpr = pTerm `chainl1` addop

pTerm :: Parser Expr
pTerm = pFactor `chainl1` mulop

pFactor :: Parser Expr
pFactor = pFloat <|> pInt <|> pParens pExpr <|> pNeg <|> pAbs <|> pMod <|> pPow
  where
    pFloat = do
        n <- float
        return $ ValFloat n
    pInt = do
        n <- integer
        return $ ValInt n
    pParens p = do
        symbol "("
        x <- p
        symbol ")"
        return x
    pNeg = do
        char '-'
        x <- pFactor
        return $ Neg x
    pAbs = do
        string "abs"
        space
        x <- pFactor
        return $ Abs x
    pMod = do
        string "mod"
        space
        x <- pFactor
        symbol ","
        y <- pFactor
        return $ Mod x y
    pPow = do
        x <- pFactor
        symbol "^"
        y <- pFactor
        return $ Pow x y

addop :: Parser (Expr -> Expr -> Expr)
addop = do{ symbol "+"; return Add } <|> do{ symbol "-"; return Sub }

mulop :: Parser (Expr -> Expr -> Expr)
mulop = do{ symbol "*"; return Mul } <|> do{ symbol "/"; return Div }

