module Parser.Parser where

import Data.Char

import Parser.LParse
import CodeModel.Function
import CodeModel.Interface
import CodeModel.Signature
import CodeModel.Type

parseName :: Parser r String
parseName = plus $ cParse (\s -> not (null s) && isLetter (head s)) (charParse id) "Expected Name"

parseType :: Parser r Type
parseType = eatWhitespace <.const id.> (parseFunctionType <|> parseNonFuncType) <.const.> eatWhitespace

parseNonFuncType :: Parser r Type
parseNonFuncType = eatWhitespace <.const id.> (parseListType <|> parseMapType <|> parseTupleType <|> parseParens <|> parsePointerType <|> parseConstType <|> parseUserDefinedType) <.const.> eatWhitespace

parseFunctionType :: Parser r Type
parseFunctionType = parseParameter <. FunctionType .> (consume "->" <.const id.> parseType)

parseParameter :: Parser r Parameter
parseParameter = (parseNonFuncType <. NamedParam .> parseName <.const.> eatWhitespace) <|> fmap Param parseNonFuncType

parseParens :: Parser r Type
parseParens = dPrefixParse "(" parseType <.const.> consume ")"

parseUserDefinedType :: Parser r Type
parseUserDefinedType = fmap UserDefined parseName

parseListType :: Parser r Type
parseListType = fmap ListType $ dPrefixParse "[" (parseType <.const.> remCB)
    where remCB = consume "]"

parseTupleType :: Parser r Type
parseTupleType = fmap TupleType $ dPrefixParse "(" (((parseType <.(:).> plus (consume "," <.const id.> parseType)) <|> pEmpty) <.const.> remCB)
    where remCB = consume ")"

parseMapType :: Parser r Type
parseMapType = dPrefixParse "{" (parseType <. MapType .> remComma <.const id.> parseType <.const.> remCB)
    where remCB = consume "}"
          remComma = consume ","

parsePointerType :: Parser r Type
parsePointerType = parseSharedType <|> parseUniqueType <|> parseViewType

parseSharedType :: Parser r Type
parseSharedType = fmap (PointerType Shared) $ dPrefixParse "shared<" parseType <.const.> consume ">"

parseUniqueType :: Parser r Type
parseUniqueType = fmap (PointerType Unique) $ dPrefixParse "unique<" parseType <.const.> consume ">"

parseViewType :: Parser r Type
parseViewType = fmap (PointerType View) $ dPrefixParse "view<" parseType <.const.> consume ">"

parseConstType :: Parser r Type
parseConstType = fmap ConstType $ dPrefixParse "const<" parseType <.const.> consume ">"

parseSignature :: Parser r Signature
parseSignature = parseName <. Signature .> (eatWhitespace <.const.> consume "::" <.const.> eatWhitespace) <. const id .> parseType

parseInterface :: Parser r Interface
parseInterface =
	(consume "interface" <.const.> eatWhitespace) <.const id.>
	parseName <. Interface .> consume "\n" <.const id.> parseBlock parseSignature

parseBlock :: Parser r a -> Parser r [a]
parseBlock p = fmap (map (const ' ')) (star $ consume " ") >>= parseBlockRest
	where parseBlockRest s = p <.(:).> ((consume "\n" <.const id.> consume s <.const id.> parseBlockRest s) <|> pEmpty)

parseFunction :: Parser r Function
parseFunction = parseSignature <. Function .> consume "\n" <.const id.> parseBlock parseStatement

parseStatement :: Parser r Statement
parseStatement = parseVarDefinition <|> parseVar

parseVarDefinition :: Parser r Statement
parseVarDefinition = (parseName <.const.> eatWhitespace <.const.> consume ":=" <.const.> eatWhitespace) <. VarDefinition .> parseStatement 

parseVar :: Parser r Statement
parseVar = fmap Var parseName