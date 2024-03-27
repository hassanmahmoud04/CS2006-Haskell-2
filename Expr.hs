module Expr where

import Parsing
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.Fixed
import Data.HashMap



type Name = String

-- Expanded to include multiple instances for Integers, Strings & Floats
data Value = IntVal Int | StrVal String | FloatVal Float
  deriving (Eq)

-- Added so output is 'x' not e.g. 'IntVal x'
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
          | Less Expr Expr
          | More Expr Expr

-- Allows for explicit show instances for Negative literals and operations 
instance Show Expr where
    show (Neg a) = show a
    show (Var a) = show a
    show (Val a) = show a
    show (Abs a) = show a 
    show (Power a b) = (show a) ++ "^" ++ (show b)
    show (Multiply a b) = (show a) ++ "*" ++ (show b)
    show (Stringconcat a b) = (show a) ++ "++" ++ (show b)

-- These are the REPL commands, expanded to include extra commands
data Command = Set Name Expr 
             | Print Expr    
             | Read Expr
             | If Expr Command Command
             | Repeat Int [Command]
             | For Command Expr Command [Command]
             | SetFunc Name [Command]
             | RunFunc Name
  deriving Show



-- 3 different functions in order to perform binary operations on integers, strings and floats

integerop :: (Int -> Int -> Int) -> Value -> Value -> Either String Value
integerop f (IntVal x) (IntVal y) =  Right $ IntVal $ f x y
integerop f _ _ = Left "Not two integer values" -- not two integer values

stringop ::(String -> String -> String) -> Value -> Value -> Either String Value
stringop f (StrVal x) (StrVal y) =  Right $ StrVal $ f x y
stringop f _ _ = Left "Not two string values"

floatop :: (Float -> Float -> Float) -> Value -> Value -> Either String Value
floatop f (FloatVal x) (FloatVal y) = Right $ FloatVal $ f x y
floatop f _ _ = Left "Not two float values"


-- Integral function in order to convert multiple integer components to floats
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

-- used to evaluate functions based on the input name, returning the commands to carry out
funcEval :: Map Name [Command] -> Name -> Maybe [Command]
funcEval funcs n = Data.HashMap.lookup n funcs

-- evaluation functions in order to carry out operations on values and return results
-- each contains different cases for integers, floats and strings
eval :: Map Name Value -> Expr -> Either String Value
eval _ (Val x) = Right x -- generic eval case
eval vars (Add x y) = do -- Adds x and y
    a <- eval vars x
    b <- eval vars y
    case (a, b) of
        (IntVal i, IntVal j) -> integerop (+) a b
        (FloatVal i, FloatVal j) -> floatop (+) a b
        _ -> Left "Values must be consistent number types"
eval vars (Subtract x y) = do  -- Subtracts y from x
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (-) a b
        (FloatVal i, FloatVal j) -> floatop (-) a b
        _ -> Left "Values must be consistent number types" 
eval vars (Multiply x y) = do  -- Product of x and y
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (*) a b 
        (FloatVal i, FloatVal j) -> floatop (*) a b
        _ -> Left "Values must be consistent number types" 
eval vars (Divide x y) = do  -- Divides x by y, using a different div function for floats and integers
    a <- eval vars x
    b <- eval vars y
    if b == IntVal(0) then Left "Division by zero"  -- Guard against division by zero
        else case (a, b) of 
            (IntVal i, IntVal j) -> integerop (div) a b 
            (FloatVal i, FloatVal j) -> floatop (/) a b
            _ -> Left "Values must be consistent number types" 
eval vars (Stringconcat x y) = do -- concatenates two strings
    a <- eval vars x
    b <- eval vars y
    case (a, b) of
        (StrVal i, StrVal j) -> stringop (++) a b
        _ -> Left "Values must be consistent string types"
eval vars (Var n) = do 
    case (Data.HashMap.lookup n vars) of -- checks against variable names and either returns variable value or error
        Just x -> Right x
        Nothing -> Left $ "Variable " ++ n ++ " not defined"
eval vars (ToString x) = do -- converts floats / integers into strings
    a <- eval vars x
    case a of
        IntVal i -> Right $ StrVal $ show i
        StrVal s -> Right $ StrVal s
        FloatVal s -> Right $ StrVal $ show s
eval vars (ToInt x) = do -- rounds floats to integers, if possible converts strings to integers
    a <- eval vars x
    case a of
        StrVal s -> do
            case (readMaybe s) of
                Just n -> return (IntVal n)
                Nothing -> Left "Not an integer"
        IntVal i -> 
            Right $ IntVal i
        FloatVal f -> 
            Right $ IntVal $ round f      
eval vars (Abs x) = do -- returns absolute value of ints and floats
    a <- eval vars x
    case a of
        IntVal i -> Right $ IntVal $ abs i
        FloatVal i -> Right $ FloatVal $ abs i
        _ -> Left "Not a number"  -- or handle other types as needed
eval vars (Mod x y) = do -- returns x mod y, using correct functions for ints and floats
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (mod) a b 
        (FloatVal i, FloatVal j) -> floatop (mod') a b
        _ -> Left "Values must be consistent number types" 
eval vars (Power x y) = do -- returns x^y, using correct functions for ints and floats
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j) -> integerop (^) a b 
        (FloatVal i, FloatVal j) -> floatop (**) a b
        _ -> Left "Values must be consistent number types" 
eval vars (ToFloat x) = do -- converts integers to floats and strings to floats if possible
    a <- eval vars x
    case a of
        StrVal s -> do
            case (readMaybe s) of
                Just n -> return (FloatVal n)
                Nothing -> Left "Not a floating point number"
        IntVal i -> 
            Right $ FloatVal $ fromIntegral i
        FloatVal f -> 
            Right $ FloatVal f
eval vars (Neg x) = do -- inverts sign of integers and floats
    a <- eval vars x
    case a of
        IntVal i -> Right $ IntVal (-i)
        FloatVal i -> Right $ FloatVal (-i)
        _ -> Left "Not a number" 
eval vars (Equals x y) = do -- returns 1 if two values are equal and 0 otherwise
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j)
            | i == j ->     Right $ IntVal 1
        (FloatVal i, FloatVal j)
            | i == j ->     Right $ IntVal 1
        (StrVal i, StrVal j)
            | i == j ->     Right $ IntVal 1
        _ -> Right $ IntVal 0
eval vars (Less x y) = do -- returns 1 if a < b and 0 otherwise
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j)
            | i < j ->     Right $ IntVal 1
        (FloatVal i, FloatVal j)
            | i < j ->     Right $ IntVal 1
        _ -> Right $ IntVal 0
eval vars (More x y) = do -- returns 1 if a > b and 0 otherwise
    a <- eval vars x
    b <- eval vars y
    case (a, b) of 
        (IntVal i, IntVal j)
            | i > j ->     Right $ IntVal 1
        (FloatVal i, FloatVal j)
            | i > j ->     Right $ IntVal 1
        _ -> Right $ IntVal 0

-- parser used to turn a string of type {a ; a; a} into a list [a]
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  first <- p
  rest <- many (sep >> p)
  return (first:rest)

-- parser for repeat command
pRepeat :: Parser Command
pRepeat = do
    string "repeat"
    space 
    n <- nat -- Parse the number of repetitions.
    space .
    char '{'
    space 
    cmds <- sepBy1 pCommand (space >> char ';' >> space) -- Parse the commands inside the block, separated by semicolons and surrounded by space.
    space 
    char '}'
    return $ Repeat n cmds

-- parser for for loops
pFor :: Parser Command 
pFor = do
    string "for"
    space
    char '('
    space
    cmd <- pSet
    space 
    char ';'
    space 
    e <- pExpr
    space 
    char ';'
    space
    cmd2 <- pCommand
    space
    char ')'
    space
    char '{'
    space 
    cmds <- sepBy1 pCommand (space >> char ';' >> space) -- Parse the commands inside the block, separated by semicolons and surrounded by space.
    space 
    char '}'
    return $ For cmd e cmd2 cmds

-- parser for function definition
pSetFunc :: Parser Command
pSetFunc = do
    string "setFunc"
    space
    name <- many1 letter
    space
    char '{'
    space
    cmds <- sepBy1 pCommand (space >> char ';' >> space)
    space
    char '}'
    return $ SetFunc name cmds

-- parser for function execution
pRunFunc :: Parser Command
pRunFunc = do
    string "func"
    space
    name <- many1 letter
    return $ RunFunc name

-- parser for print command
pPrint :: Parser Command
pPrint = do
    string "print"
    space
    e <- pExpr
    return (Print e)

-- parser for variable definition
pSet :: Parser Command
pSet = do 
    t <- many1 letter
    space
    char '='
    space
    e <- pExpr
    return (Set t e)

-- parser for read command
pRead :: Parser Command
pRead = do
    string "read"
    space 
    e <- pExpr
    return (Read e)

-- parser for 'if a then x else y' commands
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

-- groups parsers for all commands cleanly together - includes handling of whitespace before commands
pCommand :: Parser Command
pCommand = space >> (
    pRepeat
    ||| pSet
    ||| pPrint
    ||| pRead
    ||| pIf
    ||| pFor
    ||| pSetFunc
    ||| pRunFunc)

-- parser for expressions, + and - are here rather than in pTerm since they are lower priority than * and /
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
            ||| do 
                space
                string "<"
                space
                e <- pExpr
                return (Less t e)
            ||| do
                space
                string ">"
                space
                e <- pExpr
                return (More t e)
            ||| return t    

-- parser for factors, includes handling to parse floats, input, strings, abs and parentheses
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
                       str <- many (alphanum ||| char '.' ||| ' ')
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

-- includes basic parsing for * / % ^ and integer literals
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


