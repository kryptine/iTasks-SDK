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

from PPrint import class Pretty(..), ::Doc, <+>, empty, int, hsep, parens, text

import iTasks
import DocumentDB
import CleanDocParser
import CleanPrettyPrinter
	
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
	>>= \path			-> findAllFiles path ".dcl"
	>>= \dclFiles 		-> updateMultipleChoice "Select modules to include in documentation" [] dclFiles (filterDefault dclFiles)
	>>= \selectedFiles	-> sequence "Generating LaTeX" (map generateTeX selectedFiles) >>= transform (printLaTeX o flatten)
	>>= \tex -> createDocumentTask "iTasks_API_documentation.tex" "application/x-tex" tex
	>>= showInformation "Download iTasks API documentation in LaTeX format" []
	>>| stop
	where
		filterDefault = filter (\f -> indexOf "\\Core" f >= 0 || indexOf "\\Common" f >= 0)

findAllFiles :: !FilePath !String -> Task [FilePath]
findAllFiles path extension
	| endsWith extension path = return [path]
	| otherwise = 
		accWorldOSError (isSubDirectory path) >>= \isSubDir ->
		if isSubDir
			(accWorldOSError (readDirectory path) >>= \entries ->
			 sequence ("Searching directory " +++ path) 
				[ findAllFiles (path </> e) extension \\ e <- entries | e <> "." && e <> ".." ] >>= transform flatten
			)
			(return [])
where
	isSubDirectory :: !String *World -> (MaybeOSError Bool, *World)
	isSubDirectory filename world
	# (res, world) = getFileInfo filename world
	| isError res = (liftError res, world)
	= (Ok (fromOk res).directory, world)

derive class iTask LaTeX

generateTeX :: !FilePath -> Task [LaTeX]
generateTeX path = accWorldError (dclToTeX path) id



:: ModuleDoc = 
	{ ident			:: !String
	, description	:: !String
	, types			:: ![TypeDefDoc]
	, functions		:: ![FunctionDoc]
	}
	
:: FunctionDoc = 
	{ ident				:: !String
	, operator			:: !Maybe Doc
	, title				:: !String
	, params			:: ![ParameterDoc]
	, description		:: !String
	, returnType		:: !Doc
	, context			:: !Doc
	, returnDescription	:: !String
	, throws			:: ![String]
	}

:: ParameterDoc = 
	{ title			:: !String
	, description	:: !String
	, type			:: !Doc
	}
	
:: TypeDefDoc = 
	{ ident				:: !String
	, type				:: !Doc
	}
	
dclToTeX :: !FilePath *World -> (MaybeErrorString [LaTeX], *World)
dclToTeX filename world
# (res, world) = documentDCL filename world
| isError res = (liftError res, world)
= (Ok (moduleToTeX (fromOk res)), world)


moduleToTeX :: !ModuleDoc -> [LaTeX]
moduleToTeX {ModuleDoc | ident, description, types, functions} = 
	[ Section ident
	, 'LaTeX'.Text description
	]
	++ flatten (map typedefToTeX types)
	++ flatten (map functionToTeX functions)

typedefToTeX :: !TypeDefDoc -> [LaTeX]
typedefToTeX { TypeDefDoc | ident, type }
	=	[ Subsection (ident +++ " type")
		, Index ident
		, CleanCode [ prettyPrint type ]
		]

functionToTeX :: !FunctionDoc -> [LaTeX]
functionToTeX {FunctionDoc | ident, operator, params, description, returnType, returnDescription, context, throws }
	=	[ Subsection (case operator of
			Just _  = ident +++ " operator"
			Nothing = ident)
		, Index ident
		, CleanCode
			[	prettyPrint 
				(	(case operator of
						Just op = parens (text ident <+> op)
						Nothing = text ident
					)
					<+> text "::"
					<+> hsep [p.ParameterDoc.type \\ p <- params]
					<+> (if (isEmpty params) empty (text " -> ")) 
					<+> returnType
					<+> context
				)
		    ]
		, 'LaTeX'.Text description
		, Paragraph "Parameters"
		, (if (isEmpty params)
			('LaTeX'.Text "(none)")
			(Itemize (map parameterToTeX params))
		  )
		, Paragraph "Returns"
		, CleanInline (prettyPrint returnType)
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
			, CleanInline (" :: " +++ prettyPrint type)
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
	# res						= case defs of
									[PD_Documentation docstr:_]	= parseModuleComment docstr
									_							= Ok emptyModuleComment
	| isError res 				= (liftError res, world)
	# moduleComment 			= fromOk res
	= (Ok	{ ModuleDoc 
			| ident = dropExtension (dropDirectory filename) 
			, description = fromMaybe "" moduleComment.ModuleComment.description 
			, functions = documentFunctions defs
			, types = documentTypeDefs defs
			}
	  , world)
	  
documentFunctions :: ![ParsedDefinition] -> [FunctionDoc]
documentFunctions [] = []
documentFunctions [PD_Documentation docstr: PD_TypeSpec pos ident prio optSymbtype specials: defs]
	# res = parseFunctionComment docstr
	# doc = case res of
		Ok doc = doc
		Error err = emptyFunctionComment
	= case documentFunction doc ident prio optSymbtype of
		Just fd = [fd:documentFunctions defs]
		Nothing = documentFunctions defs
documentFunctions [def:defs] = documentFunctions defs

documentFunction :: !FunctionComment Ident Priority ('general'.Optional SymbolType) -> Maybe FunctionDoc
documentFunction doc ident prio 'general'.No = Nothing
documentFunction doc ident prio ('general'.Yes st) = Just
	{ FunctionDoc
	| ident				= ident.id_name
	, operator			= case prio of
							Prio LeftAssoc i = Just (text "infixl" <+> int i)
							Prio RightAssoc i = Just (text "infixr" <+> int i)
							Prio NoAssoc i = Just (text "infix" <+> int i)
							NoPrio = Nothing
	, title				= ""
	, params			= [documentParameter d p \\ d <- doc.FunctionComment.params & p <- st.st_args ]
	, description		= fromMaybe "" doc.FunctionComment.description
	, returnType		= printAType False st.st_result
	, returnDescription	= fromMaybe "" doc.FunctionComment.return
	, context			= pretty st.st_context
	, throws			= doc.FunctionComment.throws
	}

documentParameter :: !ParamComment AType -> ParameterDoc
documentParameter  doc type = 
	{ ParameterDoc
	| title			= fromMaybe "(No title)" doc.ParamComment.title
	, description	= fromMaybe "(No description)" doc.ParamComment.description
	, type			= printAType True type
	}
documentTypeDefs :: ![ParsedDefinition] -> [TypeDefDoc]
documentTypeDefs [] = []
documentTypeDefs [PD_Type typedef: defs] = 
	[documentTypeDef typedef: documentTypeDefs defs]
documentTypeDefs [def: defs] = documentTypeDefs defs

documentTypeDef :: !ParsedTypeDef -> TypeDefDoc
documentTypeDef td=:{td_ident} = 
	{ TypeDefDoc
	| ident	= td_ident.id_name
	, type	= pretty td
	}

createDocumentTask :: !String !String !String -> Task Document
createDocumentTask name mime content = mkInstantTask "Create document" create
where
	create taskNr iworld
		# (res,iworld)	= createDocument name mime content iworld
		= (TaskFinished res,iworld)
