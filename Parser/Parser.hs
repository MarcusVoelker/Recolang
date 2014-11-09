module Parser.Parser where

import Control.Applicative
import Data.Char

import Utils
import Parser.LParse
import CodeModel.Core
import CodeModel.Function
import CodeModel.Interface
import CodeModel.Signature
import CodeModel.Type

parseName :: Parser r String
parseName = some $ cParse (\s -> not (null s) && isLetter (head s)) (charParse id) "Expected Name"

parseType :: Parser r Type
parseType = eatWhitespace >> (parseFunctionType <|> parseNonFuncType) << eatWhitespace

parseNonFuncType :: Parser r Type
parseNonFuncType = eatWhitespace >> (parseListType <|> parseMapType <|> parseTupleType <|> parseParens <|> parsePointerType <|> parseConstType <|> parseUserDefinedType) << eatWhitespace

parseFunctionType :: Parser r Type
parseFunctionType = parseParameter <. FunctionType .> (consume "->" >> parseType)

parseParameter :: Parser r Parameter
parseParameter = (parseNonFuncType <. NamedParam .> parseName << eatWhitespace) <|> fmap Param parseNonFuncType

parseParens :: Parser r Type
parseParens = dPrefixParse "(" parseType << consume ")"

parseUserDefinedType :: Parser r Type
parseUserDefinedType = fmap UserDefined parseName

parseListType :: Parser r Type
parseListType = fmap ListType $ dPrefixParse "[" (parseType << remCB)
    where remCB = consume "]"

parseTupleType :: Parser r Type
parseTupleType = fmap TupleType $ dPrefixParse "(" (((parseType <.(:).> some (consume "," >> parseType)) <|> empty) << remCB)
    where remCB = consume ")"

parseMapType :: Parser r Type
parseMapType = dPrefixParse "{" (parseType <. MapType .> remComma >> parseType << remCB)
    where remCB = consume "}"
          remComma = consume ","

parsePointerType :: Parser r Type
parsePointerType = parseSharedType <|> parseUniqueType <|> parseViewType

parseSharedType :: Parser r Type
parseSharedType = fmap (PointerType Shared) $ dPrefixParse "shared<" parseType << consume ">"

parseUniqueType :: Parser r Type
parseUniqueType = fmap (PointerType Unique) $ dPrefixParse "unique<" parseType << consume ">"

parseViewType :: Parser r Type
parseViewType = fmap (PointerType View) $ dPrefixParse "view<" parseType << consume ">"

parseConstType :: Parser r Type
parseConstType = fmap ConstType $ dPrefixParse "const<" parseType << consume ">"

parseSignature :: Parser r Signature
parseSignature = parseName <. Signature .> wConsume "::" >> parseType

parseInterface :: Parser r Interface
parseInterface =
	(consume "interface" << eatWhitespace) >>
	parseName <. Interface .> consume "\n" >> parseBlock parseSignature

parseBlock :: Parser r a -> Parser r [a]
parseBlock p = fmap (map (const ' ')) (many $ consume " ") >>= parseBlockRest
	where parseBlockRest s = p <.(:).> ((consume "\n" >> consume s >> parseBlockRest s) <|> pure [])

parseFunction :: Parser r Function
parseFunction = parseSignature <. Function .> consume "\n" >> parseBlock parseStatement

parseStatementT :: Parser r Statement
parseStatementT = parseIfE <|> parseIf <|> parseWhile <|> parseFor <|> parseMapLiteral <|> parseTupleLiteral <|> parseArrayLiteral <|> parseCharLiteral <|> parseStringLiteral <|> parseVarDefinition <|> parseVar <|> parseFloatLiteral <|> parseIntLiteral

parseStatement :: Parser r Statement
parseStatement = (parseStatementT <. Assignment .> (wConsume "=" >> parseStatement)) <|> parseStatementT

parseVarDefinition :: Parser r Statement
parseVarDefinition = (parseName << wConsume ":=") <. VarDefinition .> parseStatement 

parseVar :: Parser r Statement
parseVar = fmap Var parseName

parseDigit :: Parser r Char
parseDigit = cParse (\s -> not (null s) && isDigit (head s)) (charParse id) "Expected Digit"

parseIntLiteral :: Parser r Statement
parseIntLiteral = fmap (IntLiteral . read) $  some parseDigit

parseEscapedChar :: Parser r Char
parseEscapedChar = consume "\\" >> charParse escape

parseChar :: Parser r Char
parseChar = parseEscapedChar <|> charParse id

parseStringLiteral :: Parser r Statement
parseStringLiteral = consume "\"" >> (fmap StringLiteral (many (cParse (/= "\"") parseChar ""))) << consume "\""

parseCharLiteral :: Parser r Statement
parseCharLiteral = consume "'" >> fmap CharLiteral parseChar << consume "'"

parseFDigit :: Parser r Char
parseFDigit = cParse (\s -> not (null s) && ((head s == '.') || isDigit (head s))) (charParse id) "Expected Digit or '.'"

parseFloatLiteral :: Parser r Statement
parseFloatLiteral = fmap (FloatLiteral . read) $ cParse ('.' `elem`) (some parseFDigit) "Expected '.' in float literal"

parseArrayLiteral :: Parser r Statement
parseArrayLiteral = wConsume "[" >> (fmap ArrayLiteral $ (sepMany "," parseStatement)) << wConsume "]"

parseTupleLiteral :: Parser r Statement
parseTupleLiteral = wConsume "(" >> (fmap TupleLiteral $ (sepMany "," parseStatement)) << wConsume ")"

parseMapLiteralEntry :: Parser r (Statement,Statement)
parseMapLiteralEntry = wConsume "(" >> (parseStatement << wConsume ",") <.(,).> parseStatement << wConsume ")"

parseMapLiteral :: Parser r Statement
parseMapLiteral = wConsume "{" >> (fmap MapLiteral $ sepMany "," parseMapLiteralEntry) << wConsume "}"

parseFor :: Parser r Statement
parseFor = wConsume "for" >> For <$> parseName <*> (wConsume "<-" >> parseStatement) <*> (eatWhitespace >> consume "\n" >> parseBlock parseStatement)

parseWhile :: Parser r Statement
parseWhile = wConsume "while" >> While <$> parseStatement <*> (eatWhitespace >> consume "\n" >> parseBlock parseStatement)

parseIf :: Parser r Statement
parseIf = wConsume "if" >> If <$> parseStatement <*> (eatWhitespace >> consume "\n" >> parseBlock parseStatement)

parseIfE :: Parser r Statement
parseIfE = wConsume "if" >> IfE <$> parseStatement <*> (eatWhitespace >> consume "\n" >> parseBlock parseStatement) <*> (wConsume "else" >> eatWhitespace >> consume "\n" >> parseBlock parseStatement)

parseCore :: Parser r Core
parseCore =
	(consume "core" << eatWhitespace) >>
	parseName <. Core .> consume "\n" >> parseBlock parseFunction

