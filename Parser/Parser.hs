module Parser.Parser where

import Control.Applicative
import Data.Char
import Data.Maybe

import Utils
import Parser.LParse
import CodeModel.Core
import CodeModel.Function
import CodeModel.Interface
import CodeModel.Module
import CodeModel.Signature
import CodeModel.Type

parseName :: Parser r String
parseName = some $ cParse (\s -> not (null s) && isLetter (head s)) (charParse id) "Expected Name"

parseType :: Parser r Type
parseType = eatLineWhitespace >> (parseFunctionType <|> parseNonFuncType) << eatLineWhitespace

parseNonFuncType :: Parser r Type
parseNonFuncType = eatLineWhitespace >> (parseListType <|> parseMapType <|> parseTupleType <|> parseParens <|> parsePointerType <|> parseConstType <|> parseUserDefinedType) << eatLineWhitespace

parseFunctionType :: Parser r Type
parseFunctionType = FunctionType <$> parseParameter <*> (consume "->" >> parseType)

parseParameter :: Parser r Parameter
parseParameter = (NamedParam <$> parseNonFuncType <*> parseName << eatLineWhitespace) <|> Param <$> parseNonFuncType

parseParens :: Parser r Type
parseParens = dPrefixParse "(" parseType << consume ")"

parseUserDefinedType :: Parser r Type
parseUserDefinedType = UserDefined <$> parseName

parseListType :: Parser r Type
parseListType = ListType <$> dPrefixParse "[" (parseType << remCB)
    where remCB = wConsume "]"

parseTupleType :: Parser r Type
parseTupleType = TupleType <$> dPrefixParse "(" (((parseType <.(:).> some (wConsume "," >> parseType)) <|> empty) << remCB)
    where remCB = wConsume ")"

parseMapType :: Parser r Type
parseMapType = dPrefixParse "{" (MapType <$> parseType <*> (remComma >> parseType << remCB))
    where remCB = wConsume "}"
          remComma = wConsume ","

parsePointerType :: Parser r Type
parsePointerType = parseSharedType <|> parseUniqueType <|> parseViewType

parseSharedType :: Parser r Type
parseSharedType = PointerType Shared <$> dPrefixParse "shared<" parseType << consume ">"

parseUniqueType :: Parser r Type
parseUniqueType = PointerType Unique <$> dPrefixParse "unique<" parseType << consume ">"

parseViewType :: Parser r Type
parseViewType = PointerType View <$> dPrefixParse "view<" parseType << consume ">"

parseConstType :: Parser r Type
parseConstType = ConstType <$> dPrefixParse "const<" parseType << consume ">"

parseSignature :: Parser r Signature
parseSignature = Signature <$> parseName <*> (wConsume "::" >> parseType)

parseInterface :: Parser r Interface
parseInterface =
	(consume "interface" << eatLineWhitespace) >>
	Interface <$> parseName <*> (consume "\n" >> ((fromMaybe []) <$> optional (parseBlock parseSignature)))

parseBlock :: Parser r a -> Parser r [a]
parseBlock p = (map (const ' ')) <$> (many $ consume " ") >>= parseBlockRest
	where parseBlockRest s = p <.(:).> ((consume "\n" >> consume s >> parseBlockRest s) <|> pure [])

parseFunction :: Parser r Function
parseFunction = Function <$> parseSignature <*> (consume "\n" >> parseBlock parseStatement)

parseStatementT :: Parser r Statement
parseStatementT = parseIfE <|> parseIf <|> parseWhile <|> parseFor <|> parseMapLiteral <|> parseTupleLiteral <|> parseArrayLiteral <|> parseCharLiteral <|> parseStringLiteral <|> parseVarDefinition <|> parseVar <|> parseFloatLiteral <|> parseIntLiteral

parseStatement :: Parser r Statement
parseStatement = (Assignment <$> parseStatementT <*> (wConsume "=" >> parseStatement)) <|> parseStatementT

parseVarDefinition :: Parser r Statement
parseVarDefinition = VarDefinition <$> (parseName << wConsume ":=") <*> parseStatement

parseVar :: Parser r Statement
parseVar = Var <$> parseName

parseDigit :: Parser r Char
parseDigit = cParse (\s -> not (null s) && isDigit (head s)) (charParse id) "Expected Digit"

parseIntLiteral :: Parser r Statement
parseIntLiteral = (IntLiteral . read) <$>  some parseDigit

parseEscapedChar :: Parser r Char
parseEscapedChar = consume "\\" >> charParse escape

parseChar :: Parser r Char
parseChar = parseEscapedChar <|> charParse id

parseStringLiteral :: Parser r Statement
parseStringLiteral = consume "\"" >> (StringLiteral <$> (many (cParse (/= "\"") parseChar ""))) << consume "\""

parseCharLiteral :: Parser r Statement
parseCharLiteral = consume "'" >> CharLiteral <$> parseChar << consume "'"

parseFDigit :: Parser r Char
parseFDigit = cParse (\s -> not (null s) && ((head s == '.') || isDigit (head s))) (charParse id) "Expected Digit or '.'"

parseFloatLiteral :: Parser r Statement
parseFloatLiteral = (FloatLiteral . read) <$> cParse ('.' `elem`) (some parseFDigit) "Expected '.' in float literal"

parseArrayLiteral :: Parser r Statement
parseArrayLiteral = wConsume "[" >> (ArrayLiteral <$> (sepMany "," parseStatement)) << wConsume "]"

parseTupleLiteral :: Parser r Statement
parseTupleLiteral = wConsume "(" >> (TupleLiteral <$> (sepMany "," parseStatement)) << wConsume ")"

parseMapLiteralEntry :: Parser r (Statement,Statement)
parseMapLiteralEntry = wConsume "(" >> (parseStatement << wConsume ",") <.(,).> parseStatement << wConsume ")"

parseMapLiteral :: Parser r Statement
parseMapLiteral = wConsume "{" >> (MapLiteral <$> sepMany "," parseMapLiteralEntry) << wConsume "}"

parseFor :: Parser r Statement
parseFor = wConsume "for" >> For <$> parseName <*> (wConsume "<-" >> parseStatement) <*> (eatLineWhitespace >> consume "\n" >> parseBlock parseStatement)

parseWhile :: Parser r Statement
parseWhile = wConsume "while" >> While <$> parseStatement <*> (eatLineWhitespace >> consume "\n" >> parseBlock parseStatement)

parseIf :: Parser r Statement
parseIf = wConsume "if" >> If <$> parseStatement <*> (eatLineWhitespace >> consume "\n" >> parseBlock parseStatement)

parseIfE :: Parser r Statement
parseIfE = wConsume "if" >> IfE <$> parseStatement <*> (eatLineWhitespace >> consume "\n" >> parseBlock parseStatement) <*> (wConsume "else" >> eatLineWhitespace >> consume "\n" >> parseBlock parseStatement)

parseCore :: Parser r Core
parseCore =
	(consume "core" << eatLineWhitespace) >>
	Core <$> parseName <*> (consume "\n" >> parseBlock parseFunction)

parseImport :: Parser r Import
parseImport = consume "import " >> eatLineWhitespace >> Import <$> parseName << eatLineWhitespace << consume "\n"

parseClasses :: Parser r ([Interface],[Core])
parseClasses = parseWEitherList parseInterface parseCore

parseModule :: Parser r Module
parseModule = do
    consume "module"
    eatLineWhitespace
    n <- parseName
    eatWhitespace
    imps <- many (parseImport << (many $ wConsume "\n"))
    (ints,cs) <- parseClasses
    return $ Module n imps ints cs
