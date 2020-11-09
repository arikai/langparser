{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    (
      interpretArgs,
      parseProgram,
      Program (..),
      Constant (..),
      Identifier (..),
      Operand (..),
      Statement (..),
      Expression (..),
      UnaryOp (..),
      BinaryOp (..),

      Warning,
      WarningReason
    ) where

import System.Environment (getArgs)

-- I import qualified so that it's clear which
-- functions are from the parsec library:

-- I am the error message infix operator, used later:

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative hiding (many, (<|>))

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)
import Data.String.Here.Interpolated (i, iTrim)
import Data.String.Here.Uninterpolated (here, hereLit)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Text.Parsec
import Text.Printf (printf)

import Data.Typeable (typeOf)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error

import Data.Char (isSpace)

import Data.Bool (bool)

data Program = Program [Statement] deriving (Show, Eq)

data Constant = Constant Integer deriving (Show, Eq)
data Identifier = Identifier String deriving (Show, Eq, Ord)
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

data Warning = Warning WarningReason SourcePos deriving Show

data WarningReason =
  Redeclared Identifier
  | Undeclared Identifier
  deriving Show

data Context = Context (Set Identifier) ([Warning]) deriving Show
newContext = Context (Set.empty) ([])
isDeclared :: Identifier -> Context -> Bool
isDeclared i (Context s _) = Set.member i s
declare :: Identifier -> Context -> Context
declare i (Context s ws) = Context (Set.insert i s) ws
addWarning :: WarningReason -> SourcePos -> Context -> Context
addWarning reason pos (Context s ws) = Context s ((Warning reason pos) : ws)
warnings (Context _ ws) = ws

cons u s = u : s
wrap s = [s]
void s = ()
void2 _ _ = ()
void3 _ _ _ = ()

spaces1 :: Parsec String Context ()
spaces1 = void <$> many1 space

newlineLessSpace :: (Stream s m Char) => ParsecT s u m Char
newlineLessSpace = satisfy (\c -> (isSpace c) && (c /= '\n'))
  <?> "space (w/o newline)"

lineterm = (oneOf ";\n")

eol :: Parsec String Context ()
eol = void <$> many1 (void3 <$> (skipMany newlineLessSpace)
                      <*> lineterm
                      <*> (skipMany newlineLessSpace))

-- <Const> ::= <Цифра> <Const> | <Цифра>
constant :: Parsec String Context Constant
constant = (Constant . read) <$> (many1 . oneOf $ ['0' .. '9'])
  <?> "numerical constant"

-- <Идент> ::= <Буква> <Идент> | <Буква>
identifier :: Parsec String Context Identifier
identifier = Identifier <$> (many1 . oneOf $ ['a'..'z'])
  <?> "identifier of alphabetic characters "

-- <Ун.оп.> ::= "-"|"not"
negation = const (Negation) <$> (char '-')
logicalNot = const (LogicalNot) <$> (string "not")

-- unaryOp :: Parsec String () Token
unaryOp = negation <|> logicalNot
  <?> "unary operation (- or not)"

-- <Бин.оп.> ::= "-" | "+" | "*" | "/" |"<"|">"|"=="
binaryOp :: Parsec String Context BinaryOp
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
  (Operand) <$> ((OpConstant <$> constant) <|> (OpIdentifier <$> identifier))

-- -- <Выражение> ::= <Ун.оп.> <Подвыражение> | <Подвыражение>
-- -- <Подвыражение> :: = ( <Выражение> ) | <Операнд> | < Подвыражение > <Бин.оп.> <Подвыражение>

expr :: Parsec String Context Expression
expr = binaryAp <|> operand

binaryAp =
  ( chainl1
      subexpr
      ( (\b -> \s1 -> \s2 -> BinaryAp b s1 s2)
          <$> try (do spaces; b <- binaryOp; spaces; return b)
      )
  )
    <?> "binary operation application in form of "
      ++ "<expression> <op> <expression"

subexpr :: Parsec String Context Expression
subexpr = (unaryAp <|> operand) <?> "operand with optional unary op"

unaryAp = (\o -> \_ -> \a -> UnaryAp o a) <$> unaryOp <*> spaces <*> subexpr


declareVar i pos c =
  case isDeclared i c of
    True -> addWarning (Redeclared i) pos c
    False -> declare i c

declaredIdentifier =
  do
    pos <- getPosition
    id <- identifier
    modifyState (declareVar id pos)
    return id

-- <Список переменных> ::= <Идент> | <Идент> , <Список переменных>
-- <Объявление переменных> ::= Var <Список переменных>
declaration :: Parsec String Context Statement
declaration = (do
  string "Var"; spaces1
  ids <- sepBy1 declaredIdentifier spaces1
  return (Declaration ids)) <?> "variable declaration: Var a b c..."


-- <Присваивание> ::= <Идент> = <Выражение>
assignment = (do
  pos <- getPosition
  var <- identifier
  spaces; char '='; spaces
  ex <- expr
  modifyState (\c -> case isDeclared var c of
                       True -> c
                       False -> addWarning (Undeclared var) pos c)
  return (Assignment var ex)) <?> "assignment: a = <expr...>"

-- -- <Оператор>::=<Присваивание> |<Сложный оператор>
-- -- <Сложный оператор> ::= <Оператор цикла>
-- -- <Оператор цикла>:: =WHILE <Выражение> DO <Оператор>
operator = (assignment <|> complexOperator)
complexOperator = cycleOperator
cycleOperator = (do
  string "WHILE"; spaces1;
  ex <- try binaryAp
  spaces1; string "DO"; spaces1
  op <- operator
  return (While ex op))
  <?> "a while loop: WHILE <expr> DO <assignment or while loop>"

line content = (\_ -> \c -> \_ -> c) <$> spaces <*> content <*> eol

data ParseResult = ParseResult Program Context deriving (Show)
-- <Список операторов> ::= <Оператор> | <Оператор> <Список операторов>
-- <Описание вычислений> ::= <Список операторов>
-- <Программа> ::= <Объявление переменных> <Описание вычислений> .
program :: Parsec String Context ParseResult
program =
  do
    dec <- many . try . line $ declaration
    calc <- many . try . line $ operator
    (spaces >> eof) <?> "end of input"
    state <- getState
    return (ParseResult (Program (dec ++ calc)) state)

red = "\x001b[31m"
green = "\x001b[32m"
yellow = "\x001b[33m"
bold = "\x001b[1m"
underline = "\x001b[4m"
reset = "\x001b[0m"
underlineSubstring :: [Char] -> Int -> Int -> [Char] -> [Char]
underlineSubstring t col length resetTo =
  let (left, rest) = splitAt (col-1) t
      (target, right) = splitAt length rest
  in left ++ underline ++ red ++ target ++ reset ++ resetTo ++ right

indentLine indent line = printf "% *s %s" (indent + 1) "|" line
highlightContext :: String -> SourcePos -> Int -> String
highlightContext text pos lookaround =
  let line = sourceLine pos
      column = sourceColumn pos
      context = take (2 * lookaround + 1) . drop (line - lookaround - 1) . lines $ text
      (left, (target : right)) = splitAt (lookaround) context
      indent = (length . show $ line) + 2
  in bold
     ++ unlines(
          (map (indentLine indent) left)
          ++ [printf " %d | %s" line (underlineSubstring target column 1 bold)]
          ++ (map (indentLine indent) right))
     ++ reset

fmtUnexpected :: [Parsec.Error.Message] -> String
fmtUnexpected msgs =
  case [s | Parsec.Error.UnExpect s <- msgs]
       ++ [s | Parsec.Error.SysUnExpect s <- msgs]
  of
    [] -> "???"
    (msg : _) -> msg

fmtExpected :: [Parsec.Error.Message] -> String
fmtExpected msgs =
  unlines ["  - " ++ s | Parsec.Error.Expect s <- msgs]

fmtWarnings :: [Warning] -> String
fmtWarnings warns =
  unlines
  . (map (\s -> " - " ++ s))
  . (map (\(Warning reason pos) ->
            case reason of
             Undeclared (Identifier id) ->
               [i|Undeclared variable used: ${id}|]
             Redeclared (Identifier id) ->
               [i|Redeclared variable: ${id}|]
                 )) $ warns

fmtWarningReason reason =
  case reason of
    Undeclared id ->
      [i|Undeclared variable used: ${id}|]
    Redeclared id ->
      [i|Redeclared variable: ${id}|]

data ReportSetting =
  SourceHighlight String
  | MessageHighlight String
  | SourceContextLookaround Int

defaultReportSettings =
  [SourceHighlight bold,
   MessageHighlight (bold ++ red),
   SourceContextLookaround 2]

findSetting _ [] = error "impossible"
findSetting f (x : xs) =
  case f x of
    Just(x) -> x
    Nothing -> findSetting f xs

sourceHighlightSetting =
  findSetting (\case (SourceHighlight v) -> Just(v); _ -> Nothing)

messageHighlightSetting =
  findSetting (\case (MessageHighlight v) -> Just(v); _ -> Nothing)

sourceContextLookaroundSetting =
  findSetting (\case (SourceContextLookaround v) -> Just(v); _ -> Nothing)


findFirst f l =
  (Maybe.fromJust) . (List.find f) $ l

report text pos message description userSettings =
     [iTrim|
${sourceHighlight}${source}:${line}:${column}${reset} ${messageHighlight}${message}${reset}
${highlightContext text pos contextLookaround}

${description}
|]
       where
            settings = (userSettings ++ defaultReportSettings)
            sourceHighlight = sourceHighlightSetting settings
            messageHighlight = messageHighlightSetting settings
            contextLookaround = sourceContextLookaroundSetting settings
            source = sourceName pos
            line = sourceLine pos
            column = sourceColumn pos

reportWarnings text warns =
  unlines
  . (map (\(Warning reason pos) -> report text pos (fmtWarningReason reason) "" sets))
  $ warns
  where sets = [
          SourceHighlight "",
          MessageHighlight yellow,
          SourceContextLookaround 1]

parseProgram :: String -> String -> Either String Program
parseProgram source text =
  case (Parsec.runParser program newContext source text) of
               Right(ParseResult prog (Context _ ws)) ->
                 case ws of
                   [] -> Right(prog)
                   warnings ->
                     Left . (reportWarnings text) $ warnings
               Left(error) ->
                 let pos = errorPos error
                     msgs = Parsec.Error.errorMessages error
                 in Left(
                   report text pos "parse error"
                     [iTrim|
${bold}Unexpected input:${reset} ${fmtUnexpected msgs}
${bold}Expected on of:${reset}
${fmtExpected msgs}
|] [])

parseText source text =
   putStrLn
  . (\case
        Right(result) ->
          [i|${bold}${source}${reset}: ${green}Correct${reset}|]
          -- show result
        Left(error) -> error)
  $ (do
        prog <- parseProgram source text
        Right(prog)
    )

-- -- For debugging purposes
-- pparse rules text =
--   putStrLn $
--     case (Parsec.runParser rules newContext "(source)" text) of
--       Right (result) ->
--         show result
--       Left (error) ->
--         show error
-- interpretArgs :: IO ()
-- interpretArgs =
--   pparse program [hereLit|
-- Var a;
-- a = ------10;
-- |]

interpretArgs :: IO ()
interpretArgs  =
  do
    files <- getArgs
    mapM (\fn -> do (readFile fn) >>= (parseText fn)) files
    return ()
