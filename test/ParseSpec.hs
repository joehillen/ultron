-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ParseSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Heredoc
-- import Test.Hspec.QuickCheck

import Parse (parseCommand)

spec :: Spec
spec =
  describe "parseCommand" $ do
    it "returns Left given empty string" $
      parseCommand `shouldFailOn` ""
    it "returns Left given space" $
      parseCommand `shouldFailOn` " "
    it "handles a single command" $
      parseCommand "a" `shouldParse` ("a", [])
    it "handles a single command with trailing whitespace" $
      parseCommand "a         " `shouldParse` ("a", [])
    it "handles one arg" $
      parseCommand "cmd b" `shouldParse` ("cmd", ["b"])
    it "handles two arg" $
      parseCommand "a b c" `shouldParse` ("a", ["b", "c"])
    it "handles extra space" $
      parseCommand "a    b" `shouldParse`  ("a", ["b"])
    it "handles leading and trailing whitespace" $
      parseCommand "    abc    b    c   " `shouldParse`  ("abc", ["b", "c"])
    it "accepts empty double quotes" $
      parseCommand [here|cmd ""|] `shouldParse`  ("cmd", [""])
    it "accepts double quoted whitespace" $
      parseCommand [here|cmd "  "|] `shouldParse`  ("cmd", ["  "])
    it "accepts double quoted strings" $
      parseCommand [here|cmd "foo bar"|] `shouldParse`  ("cmd", ["foo bar"])
    it "accepts double quoted double quotes" $
      parseCommand [here|cmd " \" "|] `shouldParse`  ("cmd", [[here| " |]])
    it "accepts literal backslash" $
      parseCommand [here|cmd \|] `shouldParse`  ("cmd", ["\\"])
    it "accepts escaped backslash in double quotes" $
      parseCommand [here|cmd "\\"|] `shouldParse`  ("cmd", ["\\"])
    it "accepts escaped backslash in single quotes" $
      parseCommand [here|cmd '\\'|] `shouldParse`  ("cmd", ["\\"])
    it "accepts double quoted single quotes" $
      parseCommand [here|cmd "'" "'"|] `shouldParse`  ("cmd", ["'", "'"])
    it "accepts single quotes" $
      parseCommand [here|cmd ' foo bar '|] `shouldParse`  ("cmd", [" foo bar "])
    it "accepts single quoted double quotes" $
      parseCommand [here|cmd '""'|] `shouldParse`  ("cmd", [[here|""|]])
    it "accepts single quoted single quotes" $
      parseCommand [here|cmd '\' \''|] `shouldParse`  ("cmd", ["' '"])
    it "accepts single quoted double quotes around words" $
      parseCommand [here|cmd '" foo bar "'|] `shouldParse`  ("cmd", [[here|" foo bar "|]])
    it "accepts single quoted double quotes around escaped single quotes" $
      parseCommand [here|cmd '" \'foo\' "' '" \'bar\' "'|]
        `shouldParse`  ("cmd", [[here|" 'foo' "|], [here|" 'bar' "|]])
    it "accepts doulbe quoted single quotes around escaped double quotes" $
      parseCommand [here|cmd "' \"foo bar\" '"|]
        `shouldParse`  ("cmd", [[here|' "foo bar" '|]])
    it "accepts single quoted double quotes around escaped single quotes then around escaped double quotes" $
      parseCommand [here|cmd '" \'foo \"bar\"\' "'|]
        `shouldParse`  ("cmd", [[here|" 'foo \"bar\"' "|]])
    it "accepts double quoted string ending in an escaped backslash" $
      parseCommand [here|cmd " foo bar\\"|]
        `shouldParse`  ("cmd", [[here| foo bar\|]])
    it "accepts single quoted string ending in an escaped backslash" $
      parseCommand [here|cmd ' foo bar\\'|]
        `shouldParse`  ("cmd", [[here| foo bar\|]])
    it "accepts a single quoted escaped backslash and double quote" $
      parseCommand [here|cmd '\\"'|]
        `shouldParse`  ("cmd", [[here|\"|]])
    it "accepts a double quoted escaped backslash and escaped double quote" $
      parseCommand [here|cmd "\\\""|]
        `shouldParse`  ("cmd", [[here|\"|]])
    it "does not interpret other escaped characters" $
      parseCommand [here|cmd "hello\nworld"|]
        `shouldParse`  ("cmd", [[here|hello\nworld|]])
