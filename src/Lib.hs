{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( someFunc
    ) where

-- I import qualified so that it's clear which
-- functions are from the parsec library:

-- I am the error message infix operator, used later:

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative hiding (many, (<|>))

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)
import Text.RawString.QQ

import Text.Parsec
import Text.Printf (printf)
-- import Text.Parsec (char, string, oneOf, choice, many, optional, chainl1, (<|>), (<?>))

-- import Data.Either
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error

import Data.Char (isSpace)

import Data.Bool (bool)

data Program = Program [Statement] deriving (Show, Eq)

data Constant = Constant Integer deriving (Show, Eq)
data Identifier = Identifier String deriving (Show, Eq)
data Operand  =
  OpConstant Constant
  | OpIdentifier Identifier
    deriving (Show, Eq)

data Statement =
  Declaration [Identifier]
  | Assignment Identifier Expression
  | While Expression Statement
  deriving (Show, Eq)

data Expression =
  Operand Operand
  | UnaryAp UnaryOp Expression
  | BinaryAp BinaryOp Expression Expression
  deriving (Show, Eq)

data UnaryOp = LogicalNot | Negation deriving (Show, Eq)
data BinaryOp = Minus | Plus | Mult | Div | Lesser | Greater | Equals
  deriving (Show, Eq)

boolToInt :: Bool -> Integer
boolToInt = (bool 1 0)

unop :: UnaryOp -> Integer -> Integer
unop LogicalNot = boolToInt . (== 0)
unop Negation = (\i -> - i)

binop :: BinaryOp -> Integer -> Integer -> Integer
binop Minus = (-)
binop Plus = (+)
binop Mult = (*)
binop Div = div
binop Lesser = \a -> \b -> (boolToInt (a < b))
binop Greater = \a -> \b -> (boolToInt (a > b))
binop Equals = \a -> \b -> (boolToInt (a == b))

-- <Выражение> ::= <Ун.оп.> <Подвыражение> | <Подвыражение>
-- <Подвыражение> :: = ( <Выражение> ) | <Операнд> | < Подвыражение > <Бин.оп.> <Подвыражение>
-- <Список переменных> ::= <Идент> | <Идент> , <Список переменных>
-- <Объявление переменных> ::= Var <Список переменных>
-- <Присваивание> ::= <Идент> = <Выражение>
-- <Оператор>::=<Присваивание> |<Сложный оператор>
-- <Сложный оператор> ::= <Оператор цикла>
-- <Оператор цикла>:: =WHILE <Выражение> DO <Оператор>
-- <Список операторов> ::= <Оператор> | <Оператор> <Список операторов>
-- <Описание вычислений> ::= <Список операторов>
-- <Программа> ::= <Объявление переменных> <Описание вычислений> .


cons u s = u : s
wrap s = [s]
void s = ()
void2 _ _ = ()
void3 _ _ _ = ()

spaces1 :: Parsec String () ()
spaces1 = void <$> many1 space

newlineLessSpace :: (Stream s m Char) => ParsecT s u m Char
newlineLessSpace = satisfy (\c -> (isSpace c) && (c /= '\n'))
  <?> "space (w/o newline)"

lineterm = (oneOf ";\n")

eol :: Parsec String () ()
eol = void <$> many1 (void3 <$> (skipMany newlineLessSpace)
                      <*> lineterm
                      <*> (skipMany newlineLessSpace))

-- <Const> ::= <Цифра> <Const> | <Цифра>
constant :: Parsec String () Constant
constant = (Constant . read) <$> (many1 . oneOf $ ['0' .. '9'])
  <?> "numerical constant"

-- <Идент> ::= <Буква> <Идент> | <Буква>
ident :: Parsec String () Identifier
ident = Identifier <$> (many1 . oneOf $ ['a'..'z'])
  <?> "identifier of alphabetic characters "

-- <Ун.оп.> ::= "-"|"not"
negation = const (Negation) <$> (char '-')
logicalNot = const (LogicalNot) <$> (string "not")

-- unaryOp :: Parsec String () Token
unaryOp = negation <|> logicalNot
  <?> "unary operation (- or not)"

-- <Бин.оп.> ::= "-" | "+" | "*" | "/" |"<"|">"|"=="
binaryOp :: Parsec String () BinaryOp
binaryOp =
  (\case
        "-" -> Minus
        "+" -> Plus
        "*" -> Mult
        "/" -> Div
        "<" -> Lesser
        ">" -> Greater
        "==" -> Equals) <$>
  (choice (map (string) ["-", "+", "*", "/", "<", ">", "=="]))
  <?> "one of binary operations: -, +, *, /, <, >, =="

-- <Операнд> ::= <Идент> | <Const>
operand =
  (OpConstant <$> constant) <|> (OpIdentifier <$> ident)

unaryAp = (\o -> \_ -> \a -> UnaryAp o a) <$> unaryOp <*> spaces <*> subexpr
-- -- <Выражение> ::= <Ун.оп.> <Подвыражение> | <Подвыражение>
expr :: Parsec String () Expression
expr = (unaryAp <|> subexpr) <?> "expression with optional unary op"

binaryAp =
  (\s1 -> \_ -> \b -> \_ -> \s2 -> BinaryAp b s1 s2) <$>
  subexpr <*> spaces <*> binaryOp <*> spaces <*> subexpr
  <?> "binary operation application in form of "++
       "<expression> <op> <expression"

-- <Подвыражение> :: = ( <Выражение> ) | <Операнд> | < Подвыражение > <Бин.оп.> <Подвыражение>
subexpr :: Parsec String () Expression
subexpr =
  (((Operand) <$> operand)
  <|> expr
  <|> binaryAp)
  <?> "one of: operand (const or id), expression, binary op application"

-- <Список переменных> ::= <Идент> | <Идент> , <Список переменных>
-- <Объявление переменных> ::= Var <Список переменных>
declaration = (do
  _ <- string "Var"
  _ <- spaces1
  ids <- sepBy1 ident spaces1
  return (Declaration ids)) <?> "variable declaration: Var a b c..."

-- <Присваивание> ::= <Идент> = <Выражение>
assignment = (do
  var <- ident
  _ <- spaces
  _ <- char '='
  _ <- spaces
  ex <- expr
  return (Assignment var ex)) <?> "assignment: a = <expr...>"

-- -- <Оператор>::=<Присваивание> |<Сложный оператор>
-- -- <Сложный оператор> ::= <Оператор цикла>
-- -- <Оператор цикла>:: =WHILE <Выражение> DO <Оператор>
operator = (assignment <|> complexOperator)
complexOperator = cycleOperator
cycleOperator = (do
  _ <- string "WHILE"
  _ <- spaces1
  ex <- expr
  _ <- spaces1
  _ <- string "DO"
  _ <- spaces1
  op <- operator
  return (While ex op))
  <?> "a while loop: WHILE <expr> DO <assignment or while loop>"

line content = (\_ -> \c -> \_ -> c) <$> spaces <*> content <*> eol

-- <Список операторов> ::= <Оператор> | <Оператор> <Список операторов>
-- <Описание вычислений> ::= <Список операторов>
-- <Программа> ::= <Объявление переменных> <Описание вычислений> .
program =
  do
    dec <- many . try . line $ declaration
    calc <- many . try . line $ operator
    _ <- (void2 <$> spaces <*> eof) <?> "end of input"
    return (Program (dec ++ calc))

red = "\x001b[31m"
bold = "\x001b[1m"
underline = "\x001b[4m"
reset = "\x001b[0m"
underlineSubstring :: [Char] -> Int -> Int -> [Char] -> [Char]
underlineSubstring t col length resetTo =
  let (left, rest) = splitAt (col-1) t
      (target, right) = splitAt length rest
  in left ++ underline ++ red ++ target ++ reset ++ resetTo ++ right

indentLine indent line = printf "% *s %s" (indent + 1) "|" line
highlightContext :: String -> Int -> Int -> Int -> String
highlightContext text line column lookaround =
  let context = take lookaround . drop (line-(div lookaround 2) - 1) . lines $ text
      (left, target : right) = splitAt (div lookaround 2) context
      indent = (length . show $ line) + 2
  in bold
     ++ unlines(
          (map (indentLine indent) left)
          ++ [printf " %d | %s" line (underlineSubstring target column 1 bold)]
          ++ (map (indentLine indent) right))
     ++ reset

fmtUnexpected :: [Parsec.Error.Message] -> String
fmtUnexpected msgs =
  case [s | Parsec.Error.UnExpect s <- msgs] of (msg : []) -> msg

fmtExpected :: [Parsec.Error.Message] -> String
fmtExpected msgs =
  unlines ["  - " ++ s | Parsec.Error.Expect s <- msgs]

pparse rules source text =
  putStrLn $ case (Parsec.parse rules source text) of
               Right(result) -> show result
               Left(error) ->
                 let pos = errorPos error
                     line = sourceLine pos
                     column = sourceColumn pos
                     msgs = Parsec.Error.errorMessages error
                 in
                   unlines
                   [
                     (printf "%s%s:%d:%d: parse error%s"
                      bold
                      source line column
                      reset),
                     highlightContext text line column 3,
                     "",
                     (printf "%sUnexpected input:%s %s"
                      bold reset (fmtUnexpected msgs)),
                     (printf "%sExpected on of:%s\n%s"
                      bold reset (fmtExpected msgs))
                   ]

someFunc :: IO ()
someFunc =
  pparse program "(source)" [r|


                   Var a b;
                   Var c d;;;


                   Var e;
                   Var f;
                   WHILE 1 DO a = 3;
                   WHILE 1 DO a = 4;
                   WHILE 1 DO a = 5
                   WHILE 1 DO a = 6
                   WHILE 1 DO a = 7
                   |]
