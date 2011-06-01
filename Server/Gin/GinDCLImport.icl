implementation module GinDCLImport

import StdEnv

import File
import FilePath
import Error
import Text
import ParserCombinators

import GinCleanParser
import GinSyntax

from general 	import 	::Optional(..)
from Heap 		import 	::Ptr
from scanner	import
						::Assoc(..),
						::Priority(..)
from syntax		import	
						::AType(..), 
						::ATypeVar,
						::AttrInequality, 
						::AttributeVar, 
						::BasicType,
						::ConsVariable,
						::DefinedSymbol,
						::Global,
						::GlobalIndex,
						::Ident(..), 
						::Index,
						::ParsedDefinition(..),
						::StrictnessList, 
						::SymbolTableEntry,
						::SymbolPtr,
						::SymbolType(..), 
						::TempVarId,
						::Type(..),
						::TypeAttribute(..),
						::TypeContext, 
						::TypeKind,
						::TypeSymbIdent,
						::TypeVar,
						::GenericCaseDef,
						::GenericDef,
						::ImportedObject,
						::ParsedImport,
						::ParsedInstanceAndMembers,
						::ClassDef,
						::FunSpecials,
						::Priority,
						::Position,
						::ParsedTypeDef,
						::Rhs,
						::ParsedExpr,
						::FunKind,
						::Import,
						::TypeDef,
						::TypeSymbIdent(..),
						::TypeSymbProperties,
						::TypeVar(..),
						::TypeVarInfoPtr,
						::TypeVarInfo,
						::RhsDefsOfType,
						instance toString BasicType
						
importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)
importDCL filename source world
# errorFilename 		= "errors"
# (ok,errorFile,world) 	= fopen errorFilename FWriteText world
| not ok 				= (Error "Failed to open errors file", world)
# (defs, errorFile) 	= parseModule source False errorFile
# (ok,world)			= fclose errorFile world
| not ok 				= (Error "Failed to close errors file", world)
# (ok, world)			= deleteFile errorFilename world

# gMod 					= mapModule (dropExtension (dropDirectory filename)) defs
= (Ok gMod, world)

mapModule :: !String [ParsedDefinition] -> GModule
mapModule name defs =
	{ GModule
	| name = name
	, types = []
	, moduleKind = GCleanModule (mapDefinitions defs)
	, imports = []
	}

mapDefinitions :: [ParsedDefinition] -> [Binding]
mapDefinitions [] = []
mapDefinitions [PD_Documentation docstr: PD_TypeSpec pos ident prio optSymbtype specials: defs]
	# res = parseDocBlock docstr
	# doc = case res of
		Ok doc = doc
		Error err = emptyDocBlock
	= case mapFunction doc ident prio optSymbtype of
		Just binding = [binding:mapDefinitions defs]
		Nothing = mapDefinitions defs
mapDefinitions [def:defs] = mapDefinitions defs

mapFunction :: !DocBlock Ident Priority (Optional SymbolType) -> Maybe Binding
mapFunction _ _	_ No       = Nothing
mapFunction _ _ _ (Yes st) | not (isTask (mapAType st.st_result)) = Nothing
mapFunction doc _ _ _
	| not doc.DocBlock.gin = Nothing
mapFunction doc ident prio (Yes st) 
	| doc.DocBlock.parallel && isJust doc.DocBlock.title && isJust doc.DocBlock.icon
	= Just	( ParallelBinding	{ split =	{ GDeclaration
											| name = "_split"
											, title = Just "Parallel split"
											, description = Just "Splits a parallel branch into multiple concurrent branches"
											, formalParams = []
											, returnType = GUndefinedTypeExpression
											, returnDescription = Nothing
											, icon = Just "parallel-split"
											, shape = Nothing
											}
								, merge =	{ GDeclaration
											| name = ident.id_name
											, title = doc.DocBlock.title
											, description = doc.DocBlock.description
											, formalParams = []
											, returnType = GUndefinedTypeExpression
											, returnDescription = Nothing
											, icon = doc.DocBlock.icon
											, shape = Nothing
											}
								, type = mapAType st.st_result
								, fixedNrBranches = case prio of
									NoPrio   = Nothing
									Prio _ _ = Just 2
								, parameterMap = case prio of
									NoPrio         = App [Var ident.id_name,Extension PBBranchList]
									Prio assoc pri = AppInfix ident.id_name (mapAssoc assoc) pri (Extension (PBBranch 0)) (Extension (PBBranch 1))
								}
			)
	where
		mapAssoc :: Assoc -> AFix
		mapAssoc LeftAssoc  = Infixl
		mapAssoc RightAssoc = Infixr
		mapAssoc NoAssoc    = Infix
mapFunction doc ident _ (Yes st)
	| not doc.DocBlock.parallel
	= (Just	( NodeBinding	
				{ NodeBinding 
				| declaration = 
					{ GDeclaration
					| name = ident.id_name
					, title = doc.DocBlock.title
					, description = doc.DocBlock.description
					, formalParams = if (length doc.DocBlock.params == length st.st_args)
						[ mapFormalParameter d p \\ d <- doc.DocBlock.params & p <- st.st_args ]
						[ mapFormalParameter	{ ParamDoc
												| name = "param" +++ toString i
												, title = Just (toString (mapAType p))
												, description = Nothing 
												} 
												p 
						  \\ p <- st.st_args & i <- [1..] ]
					, returnType = mapAType st.st_result
					, returnDescription = doc.DocBlock.return
					, icon = doc.DocBlock.icon
					, shape = Nothing//doc.DocBlock.shape
					}				
				, parameterMap = NBPrefixApp
				}
			)
		)

mapFunction _ _ _ _ = Nothing

mapFormalParameter :: !ParamDoc AType -> GFormalParameter
mapFormalParameter doc type = 
	{ GFormalParameter
	| name			= doc.ParamDoc.name
	, title			= doc.ParamDoc.title
	, description 	= doc.ParamDoc.description
	, type			= mapAType type
	}

mapAType :: AType -> GTypeExpression
mapAType atype = mapType atype.at_type

mapType :: Type -> GTypeExpression
mapType (TA ident [])  = GConstructor (ident.type_ident.id_name)
mapType (TA ident [param]) 
	| ident.type_ident.id_name == "_List" = GList (mapAType param)
mapType (TA ident params) = GTypeApplication [GConstructor ident.type_ident.id_name : map mapAType params]
mapType (TAS ident [] _)  = GConstructor (ident.type_ident.id_name)
mapType (TAS ident params _)  = GTypeApplication [GConstructor ident.type_ident.id_name : map mapAType params]
mapType (TV tv) = GTypeVariable (tv.tv_ident.id_name)
mapType (--> a b) = GFunction (mapAType a) (mapAType b)
mapType (TB bt) = GConstructor (toString bt)
mapType _ = GTypeVariable "(Unknown type)"

//Lexer for documentation blocks
:: Token	= ParamToken
			| ThrowsToken
			| ReturnToken
			| GinToken
			| TitleToken
			| IconToken
			| ParallelSplitToken
			| ParallelToken
			| ColonToken
			| TextToken !String
			| NewLineToken
			
derive gEq Token
instance == Token
where
	(==) a b = a === b

isText :: !Token -> Bool
isText (TextToken _) = True
isText _ = False

:: LexFunction :== String Int -> Maybe (Token, Int)
lex :: !String -> [Token]
lex input = (lex` 0 0 lexFunctions)
where
	lexFunctions :: [LexFunction]
	lexFunctions	=	[ lexFixed "@param "				ParamToken
						, lexFixed "@throws "				ThrowsToken
						, lexFixed "@return "				ReturnToken
						, lexFixed "@gin "					GinToken
						, lexFixed "@gin-title "			TitleToken
						, lexFixed "@gin-icon " 			IconToken
						, lexFixed "@gin-parallel "	 		ParallelToken
						, lexFixed ":"						ColonToken
						, lexFixed "\n*"					NewLineToken
						]
						
	lex` :: !Int !Int ![LexFunction] -> [Token]
	lex` offset start _ | offset >= size input
		= if (offset <> start) [TextToken (trim (input % (start, offset - 1)))] []
	lex` offset start [] = lex` (offset + 1) start lexFunctions 
	lex` offset start [f:fs]
		# text = if (offset <> start) [TextToken (trim (input % (start, offset - 1)))] []
		= case f input offset of
			Just (NewLineToken,offset) = text ++ lex` offset offset lexFunctions
			Just (token,offset) = text ++ [token : lex` offset offset lexFunctions]
			Nothing = lex` offset start fs
	 
	//Lex token of fixed size
	lexFixed chars token input offset
		| input % (offset,offset + (size chars) - 1) == chars	= Just (token, offset + size chars)
																= Nothing


//Parser for documentation blocks
:: DocBlock = 
	{ description	:: !Maybe String
	, params		:: ![ParamDoc]
	, return		:: !Maybe String
	, throws	 	:: ![String]
	, gin			:: !Bool
	, title			:: !Maybe String
	, icon			:: !Maybe String
	, parallel		:: !Bool
	, shape			:: !Maybe String
	}

:: ParamDoc = 
	{ name			:: !String
	, title			:: !Maybe String
	, description	:: !Maybe String																
	}

emptyDocBlock :: DocBlock
emptyDocBlock = 
	{ DocBlock
	| description	= Nothing
	, params		= []
	, return		= Nothing
	, throws		= []
	, gin			= True
	, title			= Nothing
	, icon			= Nothing
	, parallel		= False
	, shape			= Nothing
	}

parseDocBlock :: !String -> MaybeErrorString DocBlock
parseDocBlock str
# doc = pDocBlock (lex str)
| isEmpty doc = Error "Parse error"
= Ok (snd (hd doc))

pDocBlock :: Parser Token DocBlock
pDocBlock = begin1 pDocBlock`
where
	pDocBlock` = 
		pDescription <&> \description ->
		(<*> (pParam <!> pReturn <!> pThrows <!> pGin <!> pTitle <!> pIcon <!> pParallel)) <&> \args ->
		yield	((seq args) (description emptyDocBlock))
	
	pDescription = pText <&> \description ->
		yield (\doc -> { DocBlock | doc & description = Just description })
	pParam = symbol ParamToken &> pText <&> \title -> symbol ColonToken &> pText <&> \description -> 
		yield (\doc -> { DocBlock | doc & params = doc.params 
		++ [{ ParamDoc | name = makeIdent title, title = Just title, description = Just description }] })
	where
		makeIdent s = replaceSubString " " "_" (toLowerCase s)
	pReturn = symbol ReturnToken &> pText <&> \return ->
		yield (\doc -> { DocBlock | doc & return = Just return })
	pThrows = symbol ThrowsToken &> pText <&> \throws ->
		yield (\doc -> { DocBlock | doc & throws = doc.throws ++ [throws]})
	pGin = symbol GinToken &> pText <&> \gin ->
		yield (\doc -> { DocBlock | doc & gin = toLowerCase gin == "true" })
	pTitle = symbol TitleToken &> pText <&> \title ->
		yield (\doc -> { DocBlock | doc & title = Just title })
	pIcon = symbol IconToken &> pText <&> \icon ->
		yield (\doc -> { DocBlock | doc & icon = Just icon })
	pParallel = symbol ParallelToken &> pText <&> \parallel -> 
		yield (\doc -> { DocBlock | doc & parallel = toLowerCase parallel == "true" })
	pText = satisfy isText <@ \(TextToken t) -> t
	

