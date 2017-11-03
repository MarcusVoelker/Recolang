{-# LANGUAGE TemplateHaskell #-}

module Parser.Lexer where

import Control.Applicative hiding (Const)
import Data.Char
import Data.DeriveTH

import Utils
import Text.LParse.Parser
import Text.LParse.Atomics
import Text.LParse.Transformers

data Keyword = Module | Import | Interface | Core | Trait | Return | Shared | Unique | View | Const | For | While | If | Else deriving (Eq, Show)
data SpecialOperator = LParen | RParen deriving (Eq, Show)

data Token = Identifier String | Integer Integer | String String | Char Char | Float Double | Operator String | SpecialOperator SpecialOperator | Indentation Int | Keyword Keyword deriving (Eq, Show)

type STokenizer r = Parser r Char Token
type Tokenizer r = Parser r Char [Token]

concat :: Tokenizer r -> Tokenizer r -> Tokenizer r
concat l r = (++) <$> l <*> r

step :: STokenizer r -> Tokenizer r -> Tokenizer r
step l r = (:) <$> l <*> r

tokenizeIdentifier :: STokenizer r
tokenizeIdentifier = Identifier <$> some (cParse (\s -> not (null s) && isLetter (head s)) (tokenParse id) "Expected letter")

tokenizeInteger :: STokenizer r
tokenizeInteger = Integer . read <$> some (cParse (\s -> not (null s) && isDigit (head s)) (tokenParse id) "Expected digit")

parseEscapedChar :: Parser r Char Char
parseEscapedChar = consume "\\" >> tokenParse escape

parseChar :: Parser r Char Char
parseChar = parseEscapedChar <|> tokenParse id

tokenizeString :: STokenizer r
tokenizeString = consume "\"" >> (String <$> many (cParse (\s -> not (null s) && (head s /= '\"')) parseChar "")) << consume "\""

tokenizeChar :: STokenizer r
tokenizeChar = consume "'" >> Char <$> parseChar << consume "'"

tokenizeFloat :: STokenizer r
tokenizeFloat = Float . read <$> cParse (\s -> ('.' `elem` s) && any isDigit s) (some (cParse (\s -> not (null s) && (isDigit (head s) || head s == '.')) (tokenParse id) "Expected digit or '.'")) "Expected Float"

tokenizeOperator :: STokenizer r
tokenizeOperator = Operator <$> some (cParse (\s -> not (null s) && (head s `elem` "+-*/%<>[]{}^&|~!§$=.:,;?°")) (tokenParse id) "Expected letter")

tokenizeSpecialOperator :: STokenizer r
tokenizeSpecialOperator = (const (SpecialOperator LParen) <$> consume "(") 
                      <|> (const (SpecialOperator RParen) <$> consume ")") 

tokenizeIndentation :: STokenizer r
tokenizeIndentation = Indentation . length <$> (consume "\n" >> many (consume " " <|> consume "\t") << cPeek (/= "\n")) 

tokenizeKeyword :: STokenizer r
tokenizeKeyword = (const (Keyword Module) <$> consume "module") 
              <|> (const (Keyword Import) <$> consume "import")
              <|> (const (Keyword Interface) <$> consume "interface")
              <|> (const (Keyword Core) <$> consume "core")
              <|> (const (Keyword Trait) <$> consume "trait")
              <|> (const (Keyword Return) <$> consume "return")
              <|> (const (Keyword Shared) <$> consume "shared")
              <|> (const (Keyword Unique) <$> consume "unique")
              <|> (const (Keyword View) <$> consume "view")
              <|> (const (Keyword Const) <$> consume "const")

tokenizerStep :: STokenizer r
tokenizerStep = tokenizeInteger <|> tokenizeString <|> tokenizeChar <|> tokenizeFloat <|> tokenizeOperator <|> tokenizeSpecialOperator <|> tokenizeIndentation <|> tokenizeKeyword <|> tokenizeIdentifier <|> (consume " " >> tokenizerStep)

tokenizer :: Tokenizer r
tokenizer = many tokenizerStep

$(derive makeIs ''Token)