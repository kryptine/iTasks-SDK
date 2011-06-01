implementation module GinAbstractSyntax

import StdBool
from StdFunc import o
import StdOrdList
import StdList
import StdString

import Void

from GinParser import ::GPath(..)
import GinTypes
import GinPrinter

import Text

from iTaskClass import class iTask, gVisualize, gUpdate, gDefaultMask, gVerify, JSONEncode, JSONDecode, gEq

derive class iTask AModule, ADefinition, AExpression, ACaseAlt, AListComprehension, AGeneratorList, AGenerator, AFix

instance == AFix
where
	(==) Infixl Infixl = True
	(==) Infix  Infix  = True
	(==) Infixr Infixr = True
	(==) _      _      = False

expandModule :: AModule -> AModule
expandModule aMod 
= { AModule | aMod & definitions = map expandDefinition aMod.AModule.definitions }

expandDefinition :: ADefinition -> ADefinition
expandDefinition aDef =: { ADefinition | body }
#(accLocals, body) = case body of
                         PathContext path exp = let (accLocals, body) = expandExpression [] [] exp
                                                in (accLocals, PathContext path body)
                         otherwise            = expandExpression [] [] body
= { ADefinition | aDef 
                & body = body 
                , locals = map expandDefinition aDef.ADefinition.locals ++ reverse accLocals
                }

:: Scope :== [AIdentifier]
:: Locals :== [ADefinition]

expandExpression :: Scope Locals (AExpression Void)  -> (Locals, AExpression Void)
expandExpression scope accLocals (Unparsed s) = (accLocals, Unparsed s)
expandExpression scope accLocals (Lit s) = (accLocals, Lit s)
expandExpression scope accLocals (Var v) = (accLocals, Var v)
expandExpression scope accLocals (App exps)
	# (accLocals, exps`) = expandMap scope accLocals expandExpression exps
	= (accLocals, App exps`)
expandExpression scope accLocals (AppInfix i fix prec e1 e2)
	# (accLocals, e1`) = expandExpression scope accLocals e1
	# (accLocals, e2`) = expandExpression scope accLocals e2
	= (accLocals, AppInfix i fix prec e1` e2`)
expandExpression scope accLocals (Lambda pat exp) 
	# (accLocals, exp`) = expandExpression (bind pat scope) accLocals exp
	= (accLocals, Lambda pat exp`)
expandExpression scope accLocals (Let defs exp)
	# (accLocals, alts`) = expandMap scope accLocals expandLetDefs defs
	# (accLocals, exp`) = expandExpression scope accLocals exp
	= (accLocals, Let alts` exp`)
expandExpression scope accLocals (Case exp alts)
	# (accLocals, exp`) = expandExpression scope accLocals exp
	# (accLocals, alts`) = expandMap scope accLocals expandCaseAlt alts
	= (accLocals, Case exp` alts`)
expandExpression scope accLocals (Tuple exps)
	# (accLocals, exps`) = expandMap scope accLocals expandExpression exps
	= (accLocals, Tuple exps`)
expandExpression scope accLocals (List exps)
	# (accLocals, exps`) = expandMap scope accLocals expandExpression exps
	= (accLocals, List exps`)
expandExpression scope accLocals (ListComprehension alc)
	# (accLocals, alc`) = expandListComprehension scope accLocals alc
	= (accLocals, ListComprehension alc`)
expandExpression scope accLocals (PathContext path exp)
	# (accLocals, exp`) = expandExpression scope accLocals exp
	= makeLocal scope accLocals (PathContext path exp`)

expandLetDefs :: Scope Locals (APattern, AExpression Void) -> (Locals, (APattern, AExpression Void))
expandLetDefs scope accLocals (pat, exp) 
	# (accLocals, exp`) = expandExpression (bind pat scope) accLocals exp
	= (accLocals, (pat,exp`))

expandCaseAlt :: Scope Locals (ACaseAlt Void) -> (Locals, ACaseAlt Void)
expandCaseAlt scope accLocals (CaseAlt pat exp)
	# scope` = if (trim pat == "otherwise") scope (bind pat scope)
	# (accLocals, exp`) = expandExpression scope` accLocals exp
	= (accLocals, CaseAlt pat exp`)

expandListComprehension :: Scope Locals (AListComprehension Void) -> (Locals, (AListComprehension Void))
expandListComprehension scope accLocals { AListComprehension | output, generators, guards }
	# (accLocals, generators`, patterns) = expandGeneratorList scope accLocals generators
	# scope` = foldr bind scope patterns
	# (accLocals, guards`) = expandMap scope` accLocals expandExpression guards
	# (accLocals, output`) = expandExpression scope` accLocals output
	= (accLocals, { AListComprehension | output = output`, generators = generators`, guards = guards` })

expandGeneratorList :: Scope Locals (AGeneratorList Void) -> (Locals, AGeneratorList Void, [APattern])
expandGeneratorList scope accLocals (NestedGeneratorList generators)
	# (accLocals, generators`) = expandMap scope accLocals expandGenerator generators
	= (accLocals, NestedGeneratorList generators`, generatorPatterns generators)
expandGeneratorList scope accLocals (ParallelGeneratorList generators)
	# (accLocals, generators`) = expandMap scope accLocals expandGenerator generators
	= (accLocals, ParallelGeneratorList generators`, generatorPatterns generators)
	
generatorPatterns :: [AGenerator Void] -> [APattern]
generatorPatterns generators = map (\(Generator pat _) = pat) generators

expandGenerator :: Scope Locals (AGenerator Void) -> (Locals, AGenerator Void)
expandGenerator scope accLocals (Generator pat exp) 
	# (accLocals, exp`) = expandExpression scope accLocals exp
	= (accLocals, Generator pat exp`)

expandMap :: Scope Locals (Scope Locals a -> (Locals, b)) [a] -> (Locals, [b])
expandMap scope accLocals f []     = (accLocals, [])
expandMap scope accLocals f [x:xs]
	# (accLocals, x`) = f scope accLocals x
	# (accLocals, xs`) = expandMap scope accLocals f xs
	= (accLocals, [x`:xs`])

/**
* Adds a new identifier to scope
*/
bind :: GIdentifier Scope -> Scope
bind i scope = [i : filter (\p = p <> i) scope]

makeLocal :: Scope Locals (AExpression Void) -> (Locals, (AExpression Void))
makeLocal scope accLocals exp
#var = freeVariable scope accLocals
#def = { ADefinition 
       | name = var
       , formalParams = reverse [ { GFormalParameter | name = x , title = Nothing, description = Nothing, type = GUndefinedTypeExpression } \\ x <- scope ]
       , returnType = GUndefinedTypeExpression
       , body = exp
       , locals = []
       }
= ([def : accLocals], if (isEmpty scope) (Var var) (App [(Var var) : reverse (map Var scope)]))

/**
* Generate a new variable name which is free in Scope and Locals
*/
freeVariable :: Scope Locals -> AIdentifier
freeVariable scope locals = find 1
where
	find i  = let name = "v" +++ toString i 
	          in if (isMember name (scope ++ [ a.ADefinition.name \\ a <- locals ])) 
	                (find (inc i)) 
	                name

/**
* Pretty-printing of AModule's
*/


derive gEq PrintOption
instance == PrintOption where
	(==) a b = a === b

prettyPrintAModule :: PrintOption AModule -> String
prettyPrintAModule opt aMod = prettyPrint (printAModule opt aMod)

syntaxCheckPrintAModule :: AModule -> (String, LineMap)
syntaxCheckPrintAModule aMod = positionPrint (printAModule POSyntaxCheck (expandModule aMod))

printAModule :: PrintOption AModule -> a | Printer a
printAModule opt aMod =	
	scope (	[ printAModuleHeader opt aMod ]
			++ printAImports opt aMod.AModule.imports
			++ printATypes opt aMod.AModule.types
			++ flatten (map (printADefinition opt) aMod.AModule.definitions)
			++ printAStart opt aMod
		  )

printAModuleHeader :: PrintOption AModule -> a | Printer a
printAModuleHeader opt aMod = 
	def (	( case opt of
				PODCL = text "definition " 
				POICL = text "implementation "
				_	  = empty
			)
		  	<-> text "module" <+> text aMod.AModule.name
		)

printAImports :: PrintOption [AImport] -> [a] | Printer a
printAImports opt imports
	# standardImports = [ "StdInt"
						, "StdBool"
						, "StdString"
						, "StdList"
						, "StdOrdList"
						, "StdTuple"
						, "StdEnum"
						, "StdOverloaded"
						, "StdFile"
						]
						++
						( if (opt == POWriteDynamics) 
							["File", "Serialization"] 
							[]
						)
	//CoreCombinators is necessary for >>= and >>|, which do not require explicit imports in Gin
	= map (\i -> def (text "import" <+> text i)) (standardImports ++ removeDup (sort ["CoreCombinators" : imports]))

printATypes :: PrintOption [GTypeDefinition] -> [a] | Printer a
printATypes opt _ | opt == POICL = []
printATypes opt types = map (printGTypeDefinition) types

printADefinition :: PrintOption ADefinition -> [a] | Printer a
printADefinition opt def =
	printADefinitionComment opt def
	++ printADefinitionType def
	++ printADefinitionBody opt def

printADefinitionComment :: PrintOption ADefinition -> [a] | Printer a
printADefinitionComment PODCL def =
	[	text "/**" <$>
		text "*" <+> text def.ADefinition.name <$>
		text "*" <$>
		(if (isEmpty def.ADefinition.formalParams) 
			empty 
			( foldr (<$>) empty (map (\fp -> text "* @param" <+> text fp.GFormalParameter.name) 
						def.ADefinition.formalParams
				   )
			)
		) <$>
	//	text "* @return" <$>
		text "*/"
	]
printADefinitionComment _ _ = []

printADefinitionType :: ADefinition -> [a] | Printer a
printADefinitionType { ADefinition | name, formalParams, returnType }
	| typeIsDefined returnType && 
	  and (map (\fp = typeIsDefined fp.GFormalParameter.type) formalParams)
	  = [	def ( text name
	    	  <+> text "::"
	    	  </> if (isEmpty formalParams)
	    	  	  empty
		          ( fillSep (map (\fp = printGTypeExpression fp.GFormalParameter.type) formalParams) 
		            </> (text "->" </> space) </> empty
		          )
		      <-> printGTypeExpression returnType
		    )
		]
	| otherwise 
	  = []

printADefinitionBody :: PrintOption ADefinition -> [a] | Printer a
printADefinitionBody PODCL _ = []
printADefinitionBody opt { ADefinition | name, formalParams, body, locals } = 
	[ def (	text name
			<+> if (isEmpty formalParams) empty 
			     (fillSep (map (\fp = text fp.GFormalParameter.name) formalParams) <-> space)
			<-> char '='
			</> printAExpression opt body
		  )
	] 
	++ if (isEmpty locals) 
			[]
			[ text "where"
			, newscope (flatten ((map (printADefinition opt) locals)))
			]
      
printAStart :: PrintOption AModule -> [a] | Printer a
printAStart POSyntaxCheck _ = 
	[ def (text "Start :: Int")
	, def (text "Start = 0")
	]
printAStart POWriteDynamics aMod
	# task = hd aMod.AModule.definitions
	=	[ def (text "Start :: *World -> *World")
	 	, def (text "Start world")
		, text "# (_, world) = writeFile" </> dquotes (text (aMod.AModule.name +++ ".dyn")) 
			</> parens (text "serialize" </> text (task.ADefinition.name))
			</> text "world"
		, text "= world"
		]
printAStart _ _ = []

printAExpression :: PrintOption (AExpression Void) -> a | Printer a
printAExpression opt (Unparsed s) = parens (string s)
printAExpression opt (Lit s) = text s
printAExpression opt (Var v) = text v
printAExpression opt (App exps) = fillSep (map (printWithParens opt) exps)
printAExpression opt (AppInfix i fix prec e1 e2)
	# doc1 = case e1 of
		(AppInfix e1i e1fix e1prec _ _) = if (e1prec < prec || i == e1i && fix == Infixl) (printAExpression opt e1) (printWithParens opt e1)
		otherwise                       = printAExpression opt e1
	# doc2 = case e2 of
		(AppInfix e2i e2fix e2prec _ _) = if (e2prec < prec || i == e2i && fix == Infixr) (printAExpression opt e2) (printWithParens opt e2)
		otherwise                       = printAExpression opt e2
	= doc1 <$> text i </> doc2
printAExpression opt (Lambda pat exp) = text "\\" <-> printAPattern opt pat </> text "->" </> printAExpression opt exp
printAExpression opt (Let defs inexp) = text "let" <-> 
	newscope (map (\(pat,exp) -> text pat </> text "=" </> printAExpression opt exp) defs) <$>
	text "in" </> printAExpression opt inexp
printAExpression opt (Case exp alts) = parens (text "case" </> (printAExpression opt exp) </> (text "of") <$>
    newscope (map (\alt = printACaseAlt opt alt) alts))
printAExpression opt (Tuple exps) = parens (fillSep (punctuate comma (map (printAExpression opt) exps)))
printAExpression opt (List exps) = brackets (fillSep (punctuate comma (map (printAExpression opt) exps)))
printAExpression opt (ListComprehension alc) = printAListComprehension opt alc
printAExpression opt (PathContext path exp) | opt == POSyntaxCheck		= position path <-> printAExpression opt exp
                                            | otherwise                 = printAExpression opt exp

printWithParens :: PrintOption (AExpression Void) -> a | Printer a
printWithParens opt exp | needsParens exp = parens (printAExpression opt exp)
                        | otherwise       = printAExpression opt exp where
    needsParens :: (AExpression Void) -> Bool
    needsParens (App _ )             = True
    needsParens (AppInfix _ _ _ _ _) = True
    needsParens (Lambda _ _)         = True
    needsParens (Case _ _)           = True
    needsParens (PathContext _ exp)  = needsParens exp
    needsParens _                    = False

printAListComprehension  :: PrintOption (AListComprehension Void) -> a | Printer a
printAListComprehension opt alc = brackets
    ( (printAExpression opt alc.AListComprehension.output) 
      </> text "\\\\" 
      </> printGeneratorList opt alc.AListComprehension.generators
      </> hsep (map (\guard -> text "|" </> printAExpression opt guard) alc.AListComprehension.guards))
    
printGeneratorList :: PrintOption (AGeneratorList Void) -> a | Printer a
printGeneratorList opt (NestedGeneratorList generators) = fillSep (punctuate comma (map (printGenerator opt) generators))
printGeneratorList opt (ParallelGeneratorList generators) = fillSep (punctuate (text "&") (map (printGenerator opt) generators))

printGenerator :: PrintOption (AGenerator Void) -> a | Printer a
printGenerator opt (Generator sel exp) = printAPattern opt sel </> text "<-" </> printAExpression opt exp

printACaseAlt :: PrintOption (ACaseAlt Void) -> a | Printer a
printACaseAlt opt (CaseAlt pat exp) = printAPattern opt pat </> text "=" </> printAExpression opt exp

printAPattern :: PrintOption APattern -> a | Printer a
printAPattern opt p = text p

printAIdentifier :: PrintOption AIdentifier -> a | Printer a
printAIdentifier opt i = text i

printComment :: PrintOption String -> a | Printer a
printComment opt s = text "/*" </> text s </> text "*/"
