module Scheme.CoreSpec (spec) where
    
import Test.Hspec
import Scheme.Core

spec :: Spec
spec =
    describe "scheme parser" $ do
        it "parses strings" $ do
            readExpr "\"string\"" `shouldBe` "Found value: String \"string\""
        it "parses escaped characters" $ do
            readExpr "\"\\\"string\\\"\"" `shouldBe` "Found value: String \"\\\"string\\\"\""
            readExpr "\"\\n\"" `shouldBe` "Found value: String \"\\n\""
            readExpr "\"\\t\"" `shouldBe` "Found value: String \"\\t\""
            readExpr "\"\\r\"" `shouldBe` "Found value: String \"\\r\""
            readExpr "\"\\\\\"" `shouldBe` "Found value: String \"\\\\\""
        it "parses bools" $ do
            readExpr "#t" `shouldBe` "Found value: Bool True"
            readExpr "#f" `shouldBe` "Found value: Bool False"
        it "parses integers" $ do
            readExpr "1234" `shouldBe` "Found value: Number 1234"
            readExpr "0123" `shouldBe` "Found value: Number 123"
        it "parses integers with different bases" $ do
            readExpr "#b1101" `shouldBe` "Found value: Number 13"
            readExpr "#h5AB10" `shouldBe` "Found value: Number 371472"
            readExpr "#o56" `shouldBe` "Found value: Number 46"
            readExpr "#d123" `shouldBe` "Found value: Number 123"
