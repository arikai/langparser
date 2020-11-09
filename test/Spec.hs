{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
-- import Test.QuickCheck
import Lib

import Data.String.Here.Uninterpolated (here, hereLit)


parse text = parseProgram "(test)" text

main :: IO ()
main =
  hspec $ do
    describe "Lib.parseProgram" $ do
      it "parses var declaration" $ do
        parse "Var a b c;" `shouldBe` Right (
                  Program [Declaration [Identifier "a",
                                         Identifier "b",
                                         Identifier "c"]])

      it "parses multiple var declarations" $ do
        parse "Var a; Var b; Var c;" `shouldBe` Right(
          Program [
              Declaration [Identifier "a"],
              Declaration [Identifier "b"],
              Declaration [Identifier "c"]])
      it "parses simple assignments" $ do
        parse "Var a; a = 10;" `shouldBe` Right (
          Program [
              Declaration [Identifier "a"],
                Assignment (Identifier "a") (Operand (OpConstant (Constant 10)))
              ])
      it "parses sequential unary operations" $ do
        parse "Var a; a = ----10;" `shouldBe` Right (
          Program [
            Declaration [Identifier "a"],
            Assignment
              (Identifier "a")
              (UnaryAp Negation
               (UnaryAp Negation
                (UnaryAp Negation
                 (UnaryAp Negation (Operand (OpConstant (Constant 10)))))))
                  ])
      it "parses all unary operations" $ do
        parse "Var a; a = not -10;" `shouldBe` Right (
          Program [
              Declaration [Identifier "a"],
              Assignment
                (Identifier "a")
                (UnaryAp LogicalNot
                  (UnaryAp Negation (Operand (OpConstant (Constant 10)))))
              ])
      it "parses simple binary operation" $ do
        parse "Var a; a = 2 + 3;" `shouldBe` Right (
          Program [
              Declaration [Identifier "a"],
              Assignment
                (Identifier "a")
                (BinaryAp Plus
                  (Operand (OpConstant (Constant 2)))
                  (Operand (OpConstant (Constant 3))))
              ])
      it "parses nested binary operation left-associatively" $ do
        parse "Var a; a = 1 + 2 + 3 + 4 + 5;" `shouldBe` Right (
          Program [
              Declaration [Identifier "a"],
              Assignment
                (Identifier "a")
                (BinaryAp Plus
                 (BinaryAp Plus
                  (BinaryAp Plus
                   (BinaryAp Plus
                    (Operand (OpConstant (Constant 1)))
                    (Operand (OpConstant (Constant 2))))
                   (Operand (OpConstant (Constant 3))))
                  (Operand (OpConstant (Constant 4))))
                 (Operand (OpConstant (Constant 5))))
              ])
      it "parses all binary operations" $ do
        let op1 = (Operand (OpConstant (Constant 1)))
            op2 = (Operand (OpConstant (Constant 2)))
          in
          parse [hereLit|
                        Var a;
                        a = 1 - 2;
                        a = 1 + 2;
                        a = 1 * 2;
                        a = 1 / 2;
                        a = 1 < 2;
                        a = 1 > 2;
                        a = 1 == 2;
                        |]
          `shouldBe` Right (
              Program [
                  Declaration [Identifier "a"],
                  Assignment (Identifier "a") (BinaryAp Minus op1 op2),
                  Assignment (Identifier "a") (BinaryAp Plus op1 op2),
                  Assignment (Identifier "a") (BinaryAp Mult op1 op2),
                  Assignment (Identifier "a") (BinaryAp Div op1 op2),
                  Assignment (Identifier "a") (BinaryAp Lesser op1 op2),
                  Assignment (Identifier "a") (BinaryAp Greater op1 op2),
                  Assignment (Identifier "a") (BinaryAp Equals op1 op2)
                  ])
      it "parses while expressions" $ do
        parse "Var a; a = 0; WHILE a < 42 DO a = a+1;" `shouldBe` Right (
          Program [
              Declaration [Identifier "a"],
              Assignment
                (Identifier "a")
                (Operand (OpConstant (Constant 0))),
              While (BinaryAp Lesser
                      (Operand (OpIdentifier (Identifier "a")))
                      (Operand (OpConstant (Constant 42))))
                (Assignment (Identifier "a")
                  (BinaryAp Plus
                    (Operand (OpIdentifier (Identifier "a")))
                    (Operand (OpConstant (Constant 1)))))
              ])
      it "parses nested while expressions" $ do
        parse "Var a; WHILE 1 DO WHILE 2 DO WHILE 3 DO a = 42;"
          `shouldBe` Right (
          Program [
              Declaration [Identifier "a"],
              While (Operand (OpConstant (Constant 1)))
                (While (Operand (OpConstant (Constant 2)))
                  (While (Operand (OpConstant (Constant 3)))
                    (Assignment
                     (Identifier "a")
                     (Operand (OpConstant (Constant 42))))))
                  ])
      it "parses simple program" $ do
        parse [hereLit|
Var i inc limit;;;;;
Var cnt;

i = 0
inc = 10
limit = 10
cnt = 0

WHILE i < limit DO cnt = cnt + inc
|]
          `shouldBe` Right (
              Program [
                  Declaration [Identifier "i", Identifier "inc", Identifier "limit"],
                  Declaration [Identifier "cnt"],
                  Assignment (Identifier "i") (Operand (OpConstant (Constant 0))),
                  Assignment (Identifier "inc") (Operand (OpConstant (Constant 10))),
                  Assignment (Identifier "limit") (Operand (OpConstant (Constant 10))),
                  Assignment (Identifier "cnt") (Operand (OpConstant (Constant 0))),
                  While (BinaryAp Lesser
                          (Operand (OpIdentifier (Identifier "i")))
                          (Operand (OpIdentifier (Identifier "limit"))))
                  (Assignment (Identifier "cnt")
                    (BinaryAp Plus
                      (Operand (OpIdentifier (Identifier "cnt")))
                      (Operand (OpIdentifier (Identifier "inc")))))
                  ])
