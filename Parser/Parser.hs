{-# LANGUAGE Arrows #-}
module Parser.Parser  where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe

import Text.LParse.Parser
import Text.LParse.Atomics
import Text.LParse.Transformers

import qualified Parser.Lexer as Lexer

import CodeModel.Core
import CodeModel.Function
import CodeModel.Interface
import CodeModel.Module
import CodeModel.Signature
import CodeModel.Type

type TParser r t = Parser r [Lexer.Token] t

consumePartialOp :: String -> TParser t ()
consumePartialOp s = cParse 
    (\ts -> case ts of ((Lexer.Operator o):_) -> isPrefixOf s o && length s < length o;_->False) 
    (replace (\(Lexer.Operator o) -> Lexer.Operator (drop (length s) o)) noop) 
    ("Expected Op starting with " ++ s)

consumeOp :: String -> TParser t ()
consumeOp s = consumeSingle (Lexer.Operator s) <|> consumePartialOp s

consumeSOp :: Lexer.SpecialOperator -> TParser t ()
consumeSOp o = consumeSingle (Lexer.SpecialOperator o)

consumeKw :: Lexer.Keyword -> TParser t ()
consumeKw s = consumeSingle (Lexer.Keyword s)

consumeInd :: Int -> TParser t ()
consumeInd i = consumeSingle (Lexer.Indentation i)

consumeNewlines :: TParser t ()
consumeNewlines = many (consumeSingle (Lexer.Indentation 0)) >> noop

identifier :: TParser r String
identifier = (\(Lexer.Identifier i) -> i) <$> cParse ((&&) <$> not.null <*> Lexer.isIdentifier . head) (tokenParse id) ""

noComOperator :: TParser r String
noComOperator = (\(Lexer.Operator o) -> o) <$> cParse ((&&) <$> ((&&) <$> not.null <*> Lexer.isOperator . head) <*> (\((Lexer.Operator o):_) -> head o /= ',')) (tokenParse id) ""

operator :: TParser r String
operator = (\(Lexer.Operator o) -> o) <$> cParse ((&&) <$> not.null <*> Lexer.isOperator . head) (tokenParse id) ""

int :: TParser r Integer
int = (\(Lexer.Integer i) -> i) <$> cParse ((&&) <$> not.null <*> Lexer.isInteger . head) (tokenParse id) ""

char :: TParser r Char
char = (\(Lexer.Char c) -> c) <$> cParse ((&&) <$> not.null <*> Lexer.isChar . head) (tokenParse id) ""

string :: TParser r String
string = (\(Lexer.String c) -> c) <$> cParse ((&&) <$> not.null <*> Lexer.isString . head) (tokenParse id) ""

float :: TParser r Double
float = (\(Lexer.Float f) -> f) <$> cParse ((&&) <$> not.null <*> Lexer.isFloat . head) (tokenParse id) ""

parseType :: TParser r Type
parseType = parseFunctionType <|> parseNonFuncType

parseNonFuncType :: TParser r Type
parseNonFuncType = parseListType <|> parseMapType <|> parseTupleType <|> parseParens <|> parsePointerType <|> parseConstType <|> parseUserDefinedType

parseFunctionType :: TParser r Type
parseFunctionType = FunctionType <$> parseParameter <*> (consumeOp "->" >> parseType)

parseParameter :: TParser r Parameter
parseParameter = (NamedParam <$> parseNonFuncType <*> identifier) <|> Param <$> parseNonFuncType

parseParens :: TParser r Type
parseParens = consumeSOp Lexer.LParen >> parseType << consumeSOp Lexer.RParen

parseUserDefinedType :: TParser r Type
parseUserDefinedType = UserDefined <$> identifier

parseListType :: TParser r Type
parseListType = ListType <$> (consumeOp "[" >> parseType << consumeOp "]")

parseTupleType :: TParser r Type
parseTupleType = TupleType <$> (consumeSOp Lexer.LParen >> sepMany (consumeOp ",") parseType << consumeSOp Lexer.RParen)

parseMapType :: TParser r Type
parseMapType = consumeOp "{" >> (MapType <$> parseType <*> (consumeOp "," >> parseType << consumeOp "}"))

parsePointerType :: TParser r Type
parsePointerType = parseSharedType <|> parseUniqueType <|> parseViewType

parseSharedType :: TParser r Type
parseSharedType = PointerType Shared <$> (consumeKw Lexer.Shared >> consumeOp "<"  >> parseType << consumeOp ">")

parseUniqueType :: TParser r Type
parseUniqueType = PointerType Unique <$> (consumeKw Lexer.Unique >> consumeOp "<"  >> parseType << consumeOp ">")

parseViewType :: TParser r Type
parseViewType = PointerType View <$> (consumeKw Lexer.View >> consumeOp "<"  >> parseType << consumeOp ">")

parseConstType :: TParser r Type
parseConstType = ConstType <$> (consumeKw Lexer.Const >> consumeOp "<"  >> parseType << consumeOp ">")

parseSignature :: TParser r Signature
parseSignature = Signature <$> identifier <*> (consumeOp "::" >> parseType)

parseInd :: TParser r Int
parseInd = cParse (\t -> case head t of (Lexer.Indentation _) -> True; _ -> False) (tokenParse (\(Lexer.Indentation i) -> i)) "Expected Indentation" 

parseBlock :: TParser r a -> TParser r [a]
parseBlock p = parseInd >>= (\ind -> (:) <$> p <*> parseBlockRest ind)
    where parseBlockRest ind = (consumeInd ind >> ((:) <$> p <*> parseBlockRest ind)) <|> return []

parseFunction :: TParser r Function
parseFunction = Function <$> parseSignature <*> parseBlock parseStatement

parseStatementT :: TParser r Statement
parseStatementT = parseReturn <|> parseIfE <|> parseIf <|> parseWhile <|> parseFor <|> parseMapLiteral <|> parseTupleLiteral <|> parseArrayLiteral <|> parseCharLiteral <|> parseStringLiteral <|> parseTemplatedIdentifier <|> parseIdentifier <|> parseFloatLiteral <|> parseIntLiteral

parseStatement :: TParser r Statement
parseStatement = (Call <$> parseStatementT <*> parseStatement) <|> ((\s1 o s2 -> Operator o s1 s2) <$> parseStatementT <*> noComOperator <*> parseStatement) <|> parseStatementT

parseTemplatedIdentifier :: TParser r Statement
parseTemplatedIdentifier = TemplatedIdentifier <$> identifier <*> (consumeSOp Lexer.LParen >> parseStatement << consumeSOp Lexer.RParen)

parseIdentifier :: TParser r Statement
parseIdentifier = Identifier <$> identifier

parseIntLiteral :: TParser r Statement
parseIntLiteral = IntLiteral <$> int

parseCharLiteral :: TParser r Statement
parseCharLiteral = CharLiteral <$> char

parseStringLiteral :: TParser r Statement
parseStringLiteral = StringLiteral <$> string

parseFloatLiteral :: TParser r Statement
parseFloatLiteral = FloatLiteral <$> float

parseArrayLiteral :: TParser r Statement
parseArrayLiteral = consumeOp "[" >> (ArrayLiteral <$> sepMany (consumeOp ",") parseStatement) << consumeOp "]"

parseTupleLiteral :: TParser r Statement
parseTupleLiteral = consumeSOp Lexer.LParen >> (TupleLiteral <$> sepMany (consumeOp ",") parseStatement) << consumeSOp Lexer.RParen

parseMapLiteralEntry :: TParser r (Statement,Statement)
parseMapLiteralEntry = consumeSOp Lexer.LParen >> ((,) <$> (parseStatement << consumeOp ",") <*> parseStatement) << consumeSOp Lexer.RParen

parseMapLiteral :: TParser r Statement
parseMapLiteral = consumeOp "{" >> (MapLiteral <$> sepMany (consumeOp ",") parseMapLiteralEntry) << consumeOp "}"

parseFor :: TParser r Statement
parseFor = consumeKw Lexer.For >> For <$> identifier <*> (consumeOp "<-" >> parseStatement) <*> parseBlock parseStatement

parseWhile :: TParser r Statement
parseWhile = consumeKw Lexer.While >> While <$> parseStatement <*> parseBlock parseStatement

parseIf :: TParser r Statement
parseIf = consumeKw Lexer.If >> If <$> parseStatement <*> parseBlock parseStatement

parseIfE :: TParser r Statement
parseIfE = consumeKw Lexer.If >> IfE <$> parseStatement <*> parseBlock parseStatement <*> (consumeKw Lexer.Else >> parseBlock parseStatement)

parseReturn :: TParser r Statement
parseReturn = consumeKw Lexer.Return >> Return <$> parseStatement

parseInterface :: TParser r Interface
parseInterface =
    consumeKw Lexer.Interface >>
    Interface <$> identifier <*> (fromMaybe [] <$> optional (consumeOp ":" >> sepMany (consumeOp ",") identifier)) <*> (fromMaybe [] <$> optional (parseBlock parseSignature)) << consumeNewlines

parseCore :: TParser r Core
parseCore = consumeKw Lexer.Core >> Core <$> identifier <*> parseBlock parseFunction << consumeNewlines

parseTrait:: TParser r Trait
parseTrait =
    consumeKw Lexer.Trait >>
    Trait <$> identifier <*> (fromMaybe [] <$> optional (consumeOp ":" >> sepMany (consumeOp ",") identifier)) <*> (fromMaybe [] <$> optional (parseBlock parseSignature)) << consumeNewlines

parseImport :: TParser r Import
parseImport = consumeKw Lexer.Import >> Import <$> identifier << consumeNewlines

parseClass :: TParser r ([Interface],[Core],[Trait])
parseClass = ((\i -> ([i],[],[])) <$> parseInterface) <|> ((\c -> ([],[c],[])) <$> parseCore) <|> ((\t -> ([],[],[t])) <$> parseTrait)

parseClasses :: TParser r ([Interface],[Core],[Trait])
parseClasses = foldr (\(i,c,t) (is,cs,ts) -> (i++is,c++cs,t++ts)) ([],[],[]) <$> many parseClass

parseModule :: TParser r Module
parseModule = do
    consumeKw Lexer.Module
    n <- identifier
    consumeNewlines
    imps <- many parseImport
    (ints,cs,ts) <- parseClasses
    eoi
    return $ Module n imps ints cs ts

fullParser :: Parser r String Module
fullParser = proc s -> do
    toks <- Lexer.tokenizer -< s
    mod <- parseModule -< toks
    returnA -< mod

parseFile :: String -> Either String Module
parseFile = doParse fullParser . (++"\n")