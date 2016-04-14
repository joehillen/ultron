-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ParseSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec
import Text.Heredoc
-- import Test.Hspec.QuickCheck

import Parse (parseCommand)

spec :: Spec
spec =
  describe "parseCommand" $ do
    it "returns Left given empty string" $
      parseCommand "" `shouldSatisfy` isLeft
    it "returns Left given space" $
      parseCommand " " `shouldSatisfy` isLeft
    it "handles a single command" $
      parseCommand "a" `shouldBe` Right ("a", [])
    it "handles a single command with trailing whitespace" $
      parseCommand "a         " `shouldBe` Right ("a", [])
    it "handles one arg" $
      parseCommand "cmd b" `shouldBe` Right ("cmd", ["b"])
    it "handles two arg" $
      parseCommand "a b c" `shouldBe` Right ("a", ["b", "c"])
    it "handles extra space" $
      parseCommand "a    b" `shouldBe` Right ("a", ["b"])
    it "handles leading and trailing whitespace" $
      parseCommand "    abc    b    c   " `shouldBe` Right ("abc", ["b", "c"])
    it "accepts empty double quotes" $
      parseCommand [here|cmd ""|] `shouldBe` Right ("cmd", [""])
    it "accepts double quoted whitespace" $
      parseCommand [here|cmd "  "|] `shouldBe` Right ("cmd", ["  "])
    it "accepts double quoted strings" $
      parseCommand [here|cmd "foo bar"|] `shouldBe` Right ("cmd", ["foo bar"])
    it "accepts double quoted double quotes" $
      parseCommand [here|cmd " \" "|] `shouldBe` Right ("cmd", [[here| " |]])
    it "accepts double quoted single quotes" $
      parseCommand [here|cmd "'" "'"|] `shouldBe` Right ("cmd", ["'", "'"])
    it "accepts single quotes" $
      parseCommand [here|cmd ' foo bar '|] `shouldBe` Right ("cmd", [" foo bar "])
    it "accepts single quoted double quotes" $
      parseCommand [here|cmd '""'|] `shouldBe` Right ("cmd", [[here|""|]])
    it "accepts single quoted single quotes" $
      parseCommand [here|cmd '\' \''|] `shouldBe` Right ("cmd", ["' '"])
    it "accepts single quoted double quotes around words" $
      parseCommand [here|cmd '" foo bar "'|] `shouldBe` Right ("cmd", [[here|" foo bar "|]])
    it "accepts single quoted double quotes around escaped single quotes" $
      parseCommand [here|cmd '" \'foo\' "' '" \'bar\' "'|]
        `shouldBe` Right ("cmd", [[here|" 'foo' "|], [here|" 'bar' "|]])
    it "accepts doulbe quoted single quotes around escaped double quotes" $
      parseCommand [here|cmd "' \"foo bar\" '"|]
        `shouldBe` Right ("cmd", [[here|' "foo bar" '|]])
    it "accepts single quoted double quotes around escaped single quotes then around escaped double quotes" $
      parseCommand [here|cmd '" \'foo \"bar\"\' "'|]
        `shouldBe` Right ("cmd", [[here|" 'foo \"bar\"' "|]])
