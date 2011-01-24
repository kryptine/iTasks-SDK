implementation module GinAbstractSyntax

import GenPrint

import GinBindings
import GinFlowLibrary
import GinSyntax
import GinParser
import GinSPTree

import PPrint
import Text

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
expandGeneratorList scope accLocals (ANestedGeneratorList generators)
	# (accLocals, generators`) = expandMap scope accLocals expandGenerator generators
	= (accLocals, ANestedGeneratorList generators`, generatorPatterns generators)
expandGeneratorList scope accLocals (AParallelGeneratorList generators)
	# (accLocals, generators`) = expandMap scope accLocals expandGenerator generators
	= (accLocals, AParallelGeneratorList generators`, generatorPatterns generators)
	
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
       , formalParams = reverse [ { GFormalParameter | name = x , type = GUndefinedTypeExpression } \\ x <- scope ]
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

instance == PrintOption where
	(==) PathContexts PathContexts = True
	(==) _            _            = False

renderAModule :: [PrintOption] AModule -> String
renderAModule opt def = display (renderPretty 80.0 0 (printAModule opt def))

printAModule :: [PrintOption] AModule -> Doc
printAModule opt mod = text "module " <+> text mod.AModule.name
                       <//>
                       text "import StdFile" <//>
                       text "import StdDynamic" <//>
                       text "import StdDynamicFileIO" <//>
                       text "import iTasks" <//>
                       vsep (map printGTypeDefinition mod.AModule.types) <//>
                       vsep (map (printADefinition opt) mod.AModule.definitions) <//>
                       text "Start :: *World -> *World" <//>
                       text "Start world"  <//>
                       //TODO: Write all ADefinitions to dynamics, not only first
                       text "#(ok, world) = writeDynamic" <+> dquotes (text mod.AModule.name) 
                       <+> parens (text "dynamic" <+> text ((hd mod.AModule.definitions).ADefinition.name))
                       <+> text "world" 
                       <//>
                       text "= world"

printADefinition :: [PrintOption] ADefinition -> Doc
printADefinition opt def =: { ADefinition | name, formalParams, body, locals }
= printADefinitionType def
    </> text name
    <+> if (isEmpty formalParams) empty 
        (hsep (map (\fp = text fp.GFormalParameter.name) formalParams) <-> space)
    <-> char '='
    <+> printAExpression opt body
    <//> if (isEmpty locals) empty
        (text "where" <//> indent 4 (vsep (map (printADefinition opt) locals)))

printADefinitionType :: ADefinition -> Doc
printADefinitionType def =:{ ADefinition | name, formalParams, returnType }
	| typeIsDefined returnType && 
	  and (map (\fp = typeIsDefined fp.GFormalParameter.type) formalParams)
	  = text name
    	<+> text "::"
	    <+> hsep (map (\fp = printGTypeExpression fp.GFormalParameter.type) formalParams) 
	    <+> if (isEmpty formalParams) empty (text "->" <-> space)
	    <-> printGTypeExpression returnType
	| otherwise 
	  = empty

printAExpression :: [PrintOption] (AExpression Void) -> Doc
printAExpression opt (Unparsed s) = parens (text s)
printAExpression opt (Lit s) = text s
printAExpression opt (Var v) = text v
printAExpression opt (App exps) = hsep (map (printWithParens opt) exps)
printAExpression opt (AppInfix i fix prec e1 e2)
	# doc1 = case e1 of
		(AppInfix e1i e1fix e1prec _ _) = if (e1prec < prec || i == e1i && fix == Infixl) (printAExpression opt e1) (printWithParens opt e1)
		otherwise                       = printAExpression opt e1
	# doc2 = case e2 of
		(AppInfix e2i e2fix e2prec _ _) = if (e2prec < prec || i == e2i && fix == Infixr) (printAExpression opt e2) (printWithParens opt e2)
		otherwise                       = printAExpression opt e2
	= doc1 <+> text i <+> doc2
printAExpression opt (Lambda pat exp) = text "\\" <+> printAPattern opt pat <+> text "=" <+> printAExpression opt exp
printAExpression opt (Case exp alts) = parens (text "case" <+> (printAExpression opt exp) <+> (text "of") <//> 
    indent 4 (vsep (map (\alt = printACaseAlt opt alt) alts)))
printAExpression opt (Tuple exps) = parens (hsep (punctuate comma (map (printAExpression opt) exps)))
printAExpression opt (List exps) = brackets (hsep (punctuate comma (map (printAExpression opt) exps)))
printAExpression opt (ListComprehension alc) = printAListComprehension opt alc
printAExpression opt (PathContext path exp) | isMember PathContexts opt = printComment opt ("PATH:" +++ (toString path)) <+> printAExpression opt exp
                                            | otherwise                 = printAExpression opt exp

printWithParens :: [PrintOption] (AExpression Void) -> Doc
printWithParens opt exp | needsParens exp = parens (printAExpression opt exp)
                        | otherwise       = printAExpression opt exp where
    needsParens :: (AExpression Void) -> Bool
    needsParens (App _ )             = True
    needsParens (AppInfix _ _ _ _ _) = True
    needsParens (Lambda _ _)         = True
    needsParens (Case _ _)           = True
    needsParens (PathContext _ exp)  = needsParens exp
    needsParens _                    = False
    
printAListComprehension  :: [PrintOption] (AListComprehension Void) -> Doc
printAListComprehension opt alc = brackets
    ( (printAExpression opt alc.AListComprehension.output) 
      <+> text "\\\\" 
      <+> printGeneratorList opt alc.AListComprehension.generators
      <+> hsep (map (\guard -> text "|" <+> printAExpression opt guard) alc.AListComprehension.guards))
    
printGeneratorList :: [PrintOption] (AGeneratorList Void) -> Doc
printGeneratorList opt (ANestedGeneratorList generators) = hsep (punctuate comma (map (printGenerator opt) generators))
printGeneratorList opt (AParallelGeneratorList generators) = hsep (punctuate (text "&") (map (printGenerator opt) generators))

printGenerator :: [PrintOption] (AGenerator Void) -> Doc
printGenerator opt (Generator sel exp) = printAPattern opt sel <+> text "<-" <+> printAExpression opt exp

printACaseAlt :: [PrintOption] (ACaseAlt Void) -> Doc
printACaseAlt opt (CaseAlt pat exp) = printAPattern opt pat <+> text "=" <-> (indent 4 (printAExpression opt exp))

printAPattern :: [PrintOption] APattern -> Doc
printAPattern opt p = text p

printAIdentifier :: [PrintOption] AIdentifier -> Doc
printAIdentifier opt i = text i

printComment :: [PrintOption] String -> Doc
printComment opt s = text "/*" <+> text s <+> text "*/"

