implementation module APIDocumentation

from StdFunc import id
import StdFile
import StdList
import StdString

import Error
import Maybe
import File
import FilePath
import Directory
import Text

from LaTeX import :: LaTeX (CleanCode, CleanInline, EmDash, Emph, Index, Itemize, Item, NewParagraph, Paragraph, Section, Subsection), printLaTeX
from LaTeX import qualified :: LaTeX (Text)

import iTasks
import DocumentDB
import CleanDocParser

from general 	import 	qualified ::Optional(..)
from general	import	::Bind(..), ::Env(..), ::BITVECT
from Heap 		import 	::Ptr
from scanner	import
						::Assoc(..),
						::Priority(..)
from syntax		import	
						::Annotation,
						::AttributeVar, 
						::AttrInequality, 
						::AType(..), 
						::ATypeVar,
						::BasicType(..),
						::ClassDef,
						::ConsVariable,
						::DefinedSymbol(..),
						::FunKind,
						::FunSpecials(..),
						::GenericCaseDef,
						::GenericDef,
						::GenericTypeContext(..),
						::Global(..),
						::GlobalIndex,
						::Ident(..), 
						::Import,
						::ImportedObject,
						::Index,
						::ParsedConstructor(..),
						::ParsedDefinition(..),
						::ParsedExpr,
						::ParsedImport,
						::ParsedInstanceAndMembers,
						::ParsedSelector(..),
						::ParsedTypeDef,
						::Position,
						::Rhs,
						::RhsDefsOfType(..),
						::Special(..),
						::SpecialSubstitution(..),
						::StrictnessList, 
						::SymbolPtr,
						::SymbolTableEntry,
						::SymbolType(..), 
						::TCClass(..),
						::TempVarId,
						::Type(..),
						::TypeAttribute(..),
						::TypeContext(..), 
						::TypeDef(..),
						::TypeKind,
						::TypeSymbIdent(..),
						::TypeSymbProperties,
						::TypeVar(..),
						::TypeVarInfo,
						::TypeVarInfoPtr,
						::VarInfo,
						::VarInfoPtr,
						instance toString BasicType

apiDocumentationExamples :: [Workflow]
apiDocumentationExamples = 
	[ workflow	"Examples/Miscellaneous/Generate API documentation" "Generate iTasks API documentation in LaTeX format" generateTeXExample ]

generateTeXExample :: Task Void	
generateTeXExample = updateInformation "Enter API Directory:" [] (".." </> "Server" </> "API")
	>>= \directory -> generateTeX directory >>= transform printLaTeX 
	>>= \tex -> createDocumentTask "iTasks_API_documentation.tex" "application/x-tex" tex
	>>= showInformation "Download iTasks API documentation in LaTeX format" []
	>>| stop
	
derive class iTask LaTeX

generateTeX :: !FilePath -> Task [LaTeX]
generateTeX path
| endsWith ".dcl" path =
	accWorldError (documentDCL path) id >>= \moduleDoc ->
	return (moduleToTeX moduleDoc)
| otherwise = 
	accWorldOSError (isSubDirectory path) >>= \isSubDir ->
	if isSubDir
		(accWorldOSError (readDirectory path) >>= \entries ->
		 sequence ("Generating TeX documentation in " +++ path)
		 	[ generateTeX (path </> e) \\ e <- entries | e <> "." && e <> ".."] >>= transform flatten
		)
		(return [])
where
	isSubDirectory :: !String *World -> (MaybeOSError Bool, *World)
	isSubDirectory filename world
	# (res, world) = getFileInfo filename world
	| isError res = (liftError res, world)
	= (Ok (fromOk res).directory, world)

:: ModuleDoc = 
	{ ident			:: !String
	, types			:: ![TypeDefDoc]
	, functions		:: ![FunctionDoc]
	}
	
:: FunctionDoc = 
	{ ident				:: !String
	, operator			:: !Maybe String
	, title				:: !String
	, params			:: ![ParameterDoc]
	, description		:: !String
	, returnType		:: !TypeDoc
	, returnDescription	:: !String
	, context			:: !Maybe String
	, throws			:: ![String]
	}

:: ParameterDoc = 
	{ title			:: !String
	, description	:: !String
	, type			:: !TypeDoc
	}
	
:: TypeDefDoc = 
	{ ident				:: !String
	, def				:: !String
	}

:: TypeDoc :== String

derive class iTask ModuleDoc, FunctionDoc, ParameterDoc, TypeDefDoc

moduleToTeX :: !ModuleDoc -> [LaTeX]
moduleToTeX {ModuleDoc | ident, types, functions} = 
	[ Section ident ]
	++ flatten (map typedefToTeX types)
	++ flatten (map functionToTeX functions)

typedefToTeX :: !TypeDefDoc -> [LaTeX]
typedefToTeX { ident, def }
	=	[ Subsection (":: " +++ ident)
		, Index ident
		, CleanCode [ def ]
		]

functionToTeX :: !FunctionDoc -> [LaTeX]
functionToTeX {FunctionDoc | ident, operator, params, description, returnType, returnDescription, context, throws }
	=	[ Subsection (case operator of
			Just op = ident +++ " operator"
			Nothing = ident)
		, Index ident
		, CleanCode
			[	(case operator of
					Just op = parens ident +++ " " +++ op
					Nothing = ident
				)
			    +++ " :: " 
				+++ join " " [p.ParameterDoc.type \\ p <- params]
				+++ (if (isEmpty params) "" " -> ") +++ returnType
				+++ (case context of
					Nothing = ""
					Just c = " | " +++ c)
		    ]
		, 'LaTeX'.Text description
		, Paragraph "Parameters"
		, (if (isEmpty params)
			('LaTeX'.Text "(none)")
			(Itemize (map parameterToTeX params))
		  )
		, Paragraph "Returns"
		, CleanInline returnType
		, EmDash
		, 'LaTeX'.Text returnDescription
		]
		++
		(if (isEmpty throws)
			[]
			[ Paragraph "Possible exceptions"
			, (Itemize [ Item ['LaTeX'.Text e] \\ e <- throws ])
			]			
		)

parameterToTeX :: !ParameterDoc -> LaTeX
parameterToTeX {ParameterDoc | title, type, description} = 
	Item	[ 'LaTeX'.Text title
			, CleanInline (" :: " +++ type)
			, EmDash
			, 'LaTeX'.Text description
			]
			
documentDCL :: !FilePath *World -> (MaybeErrorString ModuleDoc, *World)
documentDCL filename world
	# (res, world)				= readFile filename world
	| isError res				= (Error ("Failed to read file: " +++ toString (fromError res)), world)
	# (ok, errorFile, world) 	= fopen "errors.txt" FWriteText world
	| not ok					= (Error "Failed to open errors.txt file", world)
	# (defs, errorFile)			= parseModule (fromOk res) False errorFile 
	# (ok,world)				= fclose errorFile world
	| not ok					= (Error "Failed to close errors.txt file", world)
	# (_,world)					= deleteFile "errors.txt" world
	= (Ok	{ ModuleDoc 
			| ident = dropExtension (dropDirectory filename) 
			, functions = documentFunctions defs
			, types = documentTypeDefs defs
			}
	  , world)
	  
documentFunctions :: ![ParsedDefinition] -> [FunctionDoc]
documentFunctions [] = []
documentFunctions [PD_Documentation docstr: PD_TypeSpec pos ident prio optSymbtype specials: defs]
	# res = parseDocBlock docstr
	# doc = case res of
		Ok doc = doc
		Error err = emptyDocBlock
	= case documentFunction doc ident prio optSymbtype of
		Just fd = [fd:documentFunctions defs]
		Nothing = documentFunctions defs
documentFunctions [def:defs] = documentFunctions defs

documentFunction :: !DocBlock Ident Priority ('general'.Optional SymbolType) -> Maybe FunctionDoc
documentFunction doc ident prio 'general'.No = Nothing
documentFunction doc ident prio ('general'.Yes st) = Just
	{ FunctionDoc
	| ident				= ident.id_name
	, operator			= case prio of
							Prio LeftAssoc i = Just ("infixl " +++ toString i)
							Prio RightAssoc i = Just ("infixr " +++ toString i)
							Prio NoAssoc i = Just ("infix " +++ toString i)
							NoPrio = Nothing
	, title				= ""
	, params			= [documentParameter d p \\ d <- doc.DocBlock.params & p <- st.st_args ]
	, description		= fromMaybe "(No description)" doc.DocBlock.description
	, returnType		= printAType False st.st_result
	, returnDescription	= fromMaybe "(No return)" doc.DocBlock.return
	, context			= printContexts st.st_context
	, throws			= doc.DocBlock.throws
	}

documentParameter :: !ParamDoc AType -> ParameterDoc
documentParameter  doc type = 
	{ ParameterDoc
	| title			= fromMaybe "(No title)" doc.ParamDoc.title
	, description	= fromMaybe "(No description)" doc.ParamDoc.description
	, type			= printAType True type
	}
	
documentTypeDefs :: ![ParsedDefinition] -> [TypeDefDoc]
documentTypeDefs [] = []
documentTypeDefs [PD_Type typedef: defs] = 
	[documentTypeDef typedef: documentTypeDefs defs]
documentTypeDefs [def: defs] = documentTypeDefs defs

documentTypeDef :: !ParsedTypeDef -> TypeDefDoc
documentTypeDef { td_ident, td_rhs } = 
	{ TypeDefDoc
	| ident	= td_ident.id_name
	, def	= ":: " +++ td_ident.id_name +++ " = " +++ printRhsDefsOfType td_rhs
	}

printAType :: !Bool !AType  -> String
printAType withParens atype = printType withParens atype.at_type

printType :: !Bool !Type -> String
printType withParens (TA ident []) = ident.type_ident.id_name
printType withParens (TA ident [param])
	| ident.type_ident.id_name == "_List" = "[" +++ printAType False param +++ "]"
printType withParens (TA ident params)
	| ident.type_ident.id_name % (0,5) == "_Tuple" = parens (join "," (map (printAType False) params))
printType withParens (TA ident params) = (if withParens parens id) (join " " [ident.type_ident.id_name : map (printAType True) params])
printType withParens (TAS ident params _) = printType withParens (TA ident params) // skip strictness annotations
printType withParens (TV tv) = tv.tv_ident.id_name
printType withParens (--> a b) = parens (printAType False a +++ "->" +++ printAType False b)
printType withParens (TB bt) = toString bt
printType _ _ = "(unknown type)"

printContexts :: [TypeContext] -> Maybe String
printContexts [] = Nothing
printContexts contexts = Just (join " & " (map printContext contexts))

printContext :: TypeContext -> String
printContext { tc_class, tc_types } = printTCClass tc_class +++ " " +++ join "," (map (printType False) tc_types)

printTCClass :: TCClass -> String
printTCClass (TCClass global) = printGlobalDefinedSymbol global
printTCClass (TCGeneric {GenericTypeContext | gtc_class}) = printGlobalDefinedSymbol gtc_class

printGlobalDefinedSymbol :: !(Global DefinedSymbol) -> String
printGlobalDefinedSymbol { Global | glob_object = { DefinedSymbol | ds_ident }} = ds_ident.id_name

printTypeDef :: ParsedTypeDef -> String
printTypeDef { td_ident, td_rhs } = ":: " +++ printRhsDefsOfType td_rhs

printRhsDefsOfType :: RhsDefsOfType -> String
printRhsDefsOfType	(ConsList parsedConstructors) = join "\n    | " (map printParsedConstructor parsedConstructors)
printRhsDefsOfType	(SelectorList ident typeVars isBoxed selectors) 
	= "\n {" +++ join "\n ," (map printParsedSelector selectors) +++ "}"
printRhsDefsOfType	(TypeSpec atype) = printAType False atype
printRhsDefsOfType (EmptyRhs _) = ""
printRhsDefsOfType	_ = "(Unknown type definition)"

printParsedConstructor :: ParsedConstructor -> String
printParsedConstructor { ParsedConstructor | pc_cons_ident, pc_arg_types }
	= join " " [pc_cons_ident.id_name : map (printAType False) pc_arg_types ]
	
printParsedSelector :: ParsedSelector -> String
printParsedSelector { ParsedSelector | ps_field_ident, ps_field_type } 
	= ps_field_ident.id_name +++ " :: " +++ printAType False ps_field_type

parens :: !String -> String
parens s = "(" +++ s +++ ")"

createDocumentTask :: !String !String !String -> Task Document
createDocumentTask name mime content = mkInstantTask "Create document" create
where
	create taskNr iworld
		# (res,iworld)	= createDocument name mime content iworld
		= (TaskFinished res,iworld)
