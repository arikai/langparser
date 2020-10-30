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
-- import Text.Parsec (char, string, oneOf, choice, many, optional, chainl1, (<|>), (<?>))

-- import Data.Either
import qualified Text.Parsec as Parsec

-- import Fle

parse rule text = Parsec.parse rule "(source)" text

data Token =
  Constant Integer
  | Identifier String
  | UnaryOp UnaryOp
  | BinaryOp BinaryOp
  | Assignment
  | Declaration [String]
  | While
  deriving (Show, Eq)

data Program = Program [Statement] deriving (Show, Eq)

data AConstant = AConstant Integer deriving (Show, Eq)
data AIdentifier = AIdentifier String deriving (Show, Eq)
data AOperand  =
  AOpConst AConstant
  | AOpIdent AIdentifier
    deriving (Show, Eq)

data Statement =
  ADeclaration [AIdentifier]
  | AAssignment AIdentifier
  | AWhile Expression Statement
  deriving (Show, Eq)

data Expression =
  AOperand AOperand
  | AUnaryAp UnaryOp AOperand
  | ABinaryAp BinaryOp Expression Expression
  deriving (Show, Eq)

data UnaryOp = LogicalNot | Negation deriving (Show, Eq)
data BinaryOp = Minus | Plus | Mult | Div | Lesser | Greater | Equals
  deriving (Show, Eq)

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


-- spaces1 = (\s -> ()) <$> space
spaces1 :: Parsec String () ()
spaces1 = (\_ -> ()) <$> many1 space

eol :: Parsec String () ()
eol = (\_ -> \_ -> ()) <$> spaces <*> (void <$> oneOf ";\n" <|> void <$> eof)

-- <Const> ::= <Цифра> <Const> | <Цифра>
constant :: Parsec String () Token
constant = (Constant . read) <$> (many1 . oneOf $ ['0' .. '9'])
  -- (many . oneOf $ ['0'..'9']) >>= return . Constant . strToInt

-- <Идент> ::= <Буква> <Идент> | <Буква>
ident :: Parsec String () Token
ident = Identifier <$> (many1 . oneOf $ ['a'..'z'])

-- <Ун.оп.> ::= "-"|"not"
negation = const (UnaryOp Negation) <$> (char '-')
logicalNot = const (UnaryOp LogicalNot) <$> (string "not")

-- unaryOp :: Parsec String () Token
unaryOp = negation <|> logicalNot

-- <Бин.оп.> ::= "-" | "+" | "*" | "/" |"<"|">"|"=="
binaryOp :: Parsec String () Token
binaryOp =
  (BinaryOp . \case
        "-" -> Minus
        "+" -> Plus
        "*" -> Mult
        "/" -> Div
        "<" -> Lesser
        ">" -> Greater
        "==" -> Equals) <$>
  (choice (map (string) ["-", "+", "*", "/", "<", ">", "=="]))

cons u s = u : s
wrap s = [s]
void s = ()

-- <Операнд> ::= <Идент> | <Const>
operand = constant <|> ident

-- -- <Выражение> ::= <Ун.оп.> <Подвыражение> | <Подвыражение>
expr :: Parsec String () [Token]
-- expr = ((,) <$>  unaryOp <*> operand)
expr = choice[
           cons <$> unaryOp <*> subexpr,
           subexpr
       ]

-- <Подвыражение> :: = ( <Выражение> ) | <Операнд> | < Подвыражение > <Бин.оп.> <Подвыражение>
subexpr :: Parsec String () [Token]
subexpr =
  -- (operand >>= \o -> return [o])
  (wrap <$> operand)
  <|> expr
  <|> ((\s1 -> \b -> \s2 -> s1 ++ b : s2)  <$> subexpr <*> binaryOp <*> subexpr)

-- <Список переменных> ::= <Идент> | <Идент> , <Список переменных>
-- <Объявление переменных> ::= Var <Список переменных>
declaration = do
  _ <- string "Var"
  _ <- spaces1
  ids <- sepBy1 ident spaces1
  return ids

-- <Присваивание> ::= <Идент> = <Выражение>
assignment = do
  var <- ident
  _ <- spaces
  _ <- char '='
  _ <- spaces
  ex <- expr
  return (var : Assignment : ex)

-- -- <Оператор>::=<Присваивание> |<Сложный оператор>
-- -- <Сложный оператор> ::= <Оператор цикла>
-- -- <Оператор цикла>:: =WHILE <Выражение> DO <Оператор>
operator = (assignment <|> cOperator)
cOperator = cycleOperator
cycleOperator = do
  _ <- string "WHILE"
  _ <- spaces1
  ex <- expr
  _ <- spaces1
  _ <- string "DO"
  _ <- spaces1
  op <- operator
  return (While : ex ++ op)

-- <Список операторов> ::= <Оператор> | <Оператор> <Список операторов>
-- <Описание вычислений> ::= <Список операторов>
calculation = concat <$> (endBy operator eol)

-- <Программа> ::= <Объявление переменных> <Описание вычислений> .
program =
  do
    _ <- spaces
    -- dec <- concat <$> endBy declaration eol
    -- dec <- concat <$> many ((\d -> \_ -> d) <$> declaration <*> spaces1)
    -- _ <- spaces1
    calc <- calculation
    -- _ <- spaces
    -- return (dec ++ calc)
    return calc
    -- return (dec ++ calc)


pparse p t = putStrLn $ show (Lib.parse p t)

someFunc :: IO ()
someFunc =
  pparse program [r|
                   WHILE 1 DO a = 3 a = 5
                   |]
  -- [r|
  --                  WHILE 1 DO a = 3|]
  -- [r|
  --                      a = 3
  --                      |]
  -- putStrLn $ show (Lib.parse)
  -- let myProgram =
  --       [r|
  --         Var b
  --         b  = 5
  --         |]
  -- --       -- [r|
  -- --       --   Var a b c
  -- --       --   a = 5
  -- --       --   b = 10
  -- --       --   i = 0
  -- --       --   c = 0
  -- --       --   WHILE i < b DO
  -- --       --     c = c + 1
  -- --       --   |]
  -- in putStrLn $ show (Lib.parse program myProgram)
    -- case (Text.Parsec.parse program myProgram) of
    -- Left(error) -> putStrLn(show(error))
    -- Right(result) -> putStrLn $ show(result)
