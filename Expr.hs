module Expr where

import Parsing
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.Fixed
import Data.HashMap



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
          | Neg Expr
          | Equals Expr Expr
        --deriving Show
        --   | If Expr Command Command
        --   | Then Expr
        --   | Else Expr

instance Show Expr where
    show (Neg a) = show a
    show (Var a) = show a
    show (Val a) = show a
    show (Abs a) = show a 
    show (Power a b) = (show a) ++ "^" ++ (show b)
    show (Multiply a b) = (show a) ++ "*" ++ (show b)
    show (Stringconcat a b) = (show a) ++ "++" ++ (show b)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit 
             | Read Expr
             | If Expr Command Command
             | Repeat Int [Command]
  deriving Show



    
integerop :: (Int -> Int -> Int) -> Value -> Value -> Maybe Value
integerop f (IntVal x) (IntVal y) =  Just(IntVal(f x y))
integerop f _ _ = Nothing -- not two integer values

stringop ::(String -> String -> String) -> Value -> Value -> Maybe Value
stringop f (StrVal x) (StrVal y) =  Just(StrVal(f x y))
stringop f _ _ = Nothing

floatop :: (Float -> Float -> Float) -> Value -> Value -> Maybe Value
floatop f (FloatVal x) (FloatVal y) = Just(FloatVal(f x y))
floatop f _ _ = Nothing


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


eval :: Map Name Value -> Expr -> Maybe Value
eval _ (Val x) = Just x
eval vars (Add x y) = do
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (+) a b
        (FloatVal i, FloatVal j) -> floatop (+) a b
        _ -> Nothing
eval vars (Subtract x y) = do  -- Assuming Subtract is part of Expr
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (-) a b
        (FloatVal i, FloatVal j) -> floatop (-) a b
        _ -> Nothing 
eval vars (Multiply x y) = do  -- Assuming Multiply is part of Expr
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (*) a b 
        (FloatVal i, FloatVal j) -> floatop (*) a b
        _ -> Nothing 
eval vars (Divide x y) = do  -- Assuming Divide is part of Expr
    a <- eval vars x
    b <- eval vars y
    if b == IntVal(0) then Nothing  -- Guard against division by zero
        else case (a, b) of 
            (IntVal i, IntVal j) -> integerop (div) a b 
            (FloatVal i, FloatVal j) -> floatop (/) a b
            _ -> Nothing 
eval vars (Stringconcat x y) = do 
    a <- eval vars x
    b <- eval vars y
    stringop (++) a b
eval vars (Var n) = Data.HashMap.lookup n vars
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
        FloatVal i -> Just (FloatVal (abs i))
        _ -> Nothing  -- or handle other types as needed
eval vars (Mod x y) = do
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (mod) a b 
        (FloatVal i, FloatVal j) -> floatop (mod') a b
        _ -> Nothing
eval vars (Power x y) = do
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (^) a b 
        (FloatVal i, FloatVal j) -> floatop (**) a b
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
eval vars (Neg x) = do
    a <- eval vars x
    case a of
        IntVal i -> Just (IntVal (-i))
        FloatVal i -> Just (FloatVal (-i))
        _ -> Nothing
eval vars (Equals x y) = do
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j)
            | i == j ->     Just (IntVal 1)
            | otherwise ->  Just (IntVal 0)
        (FloatVal i, FloatVal j)
            | i == j ->     Just (IntVal 1)
            | otherwise ->  Just (IntVal 0)
        (StrVal i, StrVal j)
            | i == j ->     Just (IntVal 1)
            | otherwise ->  Just (IntVal 0)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  first <- p
  rest <- many (sep >> p)
  return (first:rest)


pRepeat :: Parser Command
pRepeat = do
    string "repeat"
    spaces -- Consume any spaces after the keyword.
    n <- nat -- Parse the number of repetitions.
    spaces -- Optional: consume any spaces before the '{'.
    char '{'
    spaces -- Optional: consume any spaces before the first command.
    cmds <- sepBy1 pCommand (spaces >> char ';' >> spaces) -- Parse the commands inside the block, separated by semicolons and surrounded by spaces.
    spaces -- Optional: consume any spaces after the last command and before '}'.
    char '}'
    return $ Repeat n cmds

pPrint :: Parser Command
pPrint = do
    string "print"
    spaces
    e <- pExpr
    return (Print e)

pSet :: Parser Command
pSet = do 
    t <- many1 letter
    spaces
    char '='
    spaces
    e <- pExpr
    return (Set t e)

pQuit :: Parser Command
pQuit = do
	string "quit"
	return Quit

pRead :: Parser Command
pRead = do
    string "read"
    space 
    e <- pExpr
    return (Read e)

pIf :: Parser Command
pIf = do
    string "if"
    space
    c <- pExpr
    space
    string "then"
    space
    t <- pCommand
    space
    string "else"
    space
    e <- pCommand
    return (If c t e)

pCommand :: Parser Command
pCommand = spaces >> (
  	    pRepeat
  	||| pSet
    ||| pPrint
    ||| pRead
    ||| pIf
    ||| pQuit)




pExpr :: Parser Expr
pExpr = do t <- pTerm
           (do 
                space
                char '+'
                space
                e <- pExpr
                return (Add t e)
            ||| do 
                space 
                char '-'
                space 
                e <- pExpr
                return (Subtract t e))
            ||| do 
                space 
                string "++"
                space
                e <- pExpr
                return (Stringconcat t e)
            ||| do 
                space
                string "=="
                space
                e <- pExpr
                return (Equals t e) 
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
                       space 
                       return (Input)
                    
             ||| do 
                       string "toString"
                       space 
                       a <- pExpr
                       return (ToString a)
             ||| do 
                       string "toInt"
                       space 
                       a <- pExpr
                       return (ToInt a)
             ||| do 
                       string "toFloat"
                       space 
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
                   space
                   e <- pTerm
                   space 
                   char '|'
                   return (Abs e)
           ||| do
                   char '-'
                   space
                   e <- pTerm
                   return (Neg e)


pTerm :: Parser Expr
pTerm = do f <- pFactor
           (do space 
               char '*'
               space
               t <- pTerm
               return (Multiply f t)
            ||| do space
                   char '/'
                   space
                   t <- pTerm
                   return (Divide f t)
            ||| do space
                   char '%'
                   space
                   t <- pTerm
                   return (Mod f t)
            ||| do space
                   char '^'
                   space
                   t <- pTerm
                   space
                   return (Power f t)
            ||| return f)


