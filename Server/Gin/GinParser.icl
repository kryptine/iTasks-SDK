implementation module GinParser

import StdArray
import StdEnum
from StdFunc import o
import StdTuple

import GenPrint
import Void
import Monad
import JSON

import GinSyntax
import GinAbstractSyntax
import GinFlowLibrary
import GinSPTree
import GinORYX
import GinStorage

instance toString GPath where
	toString nodes = "/" +++ (((foldr (\a b -> a +++ "/" +++ b) "") o reverse o map toString) nodes)

instance toString GPathNode where
    toString (PNDefinition i)	= toS "definitions" i
    toString PNBody				= "body"
    toString (PNDefinition i)	= toS "nodes" i
    toString (PNEdge i)			= toS "edges" i
    toString (PNActualParam i)	= toS "actualParameters" i
    toString (PNListItem i)		= toS "items" i

toS :: !String Int -> String
toS name index = name +++ "[" +++ toString index +++ "]"
    
derive class iTask GPathNode, GParseResult, GParseState

isParseError :: (GParseResult a) -> Bool
isParseError (GError _) = True
isParseError _ = False

getParseSuccess :: (GParseResult a) -> a
getParseSuccess (GSuccess a) = a

getParseError :: (GParseResult a) -> [(GPath, String)]
getParseError (GError b) = b
	
instance Monad GParseState
where
	ret :: a -> GParseState a
	ret a = GParseState (\path = GSuccess a)
	
	(>>>) infixr 5 :: (GParseState a) (a -> GParseState b) -> GParseState b
    (>>>) (GParseState m) k = GParseState (\path = case m path of
                                         GError errors = GError errors
                                         GSuccess a = strip (k a) path) where
          strip (GParseState f) = f
                                    
parseChild :: GPathNode (GParseState a) -> GParseState a
parseChild child (GParseState f) = GParseState (\path = f [child:path])

parseMap :: (a -> GParseState b) [a] -> GParseState [b]
parseMap _ []     = ret []
parseMap f [x:xs] = f x >>> \x` = 
				                  parseMap f xs >>> \xs` = 
                                  ret [x`:xs`]

parseChildMap :: (Int -> GPathNode) (a -> GParseState b) [a] -> GParseState [b]
parseChildMap  child f xs = pm f (zip2 xs [0..]) where
    pm _ []         = ret []
    pm f [(x,n):xs] = parseChild (child n) (f x) >>> \x` =
                      pm f xs               	 >>> \xs` = 
                      ret [x`:xs`]

orElse :: (GParseState a) (GParseState a) -> GParseState a
orElse (GParseState m) (GParseState k) = 
	GParseState (\p = case (m p) of
		GSuccess a    = GSuccess a
		GError errors = k p)

parseError :: String -> GParseState a
parseError message = GParseState (\path = GError [(path,message)])

parseErrorInChild :: GPathNode String -> GParseState a
parseErrorInChild child message = parseChild child (parseError message)

parseErrorInChildren :: (Int -> GPathNode) [Int] String -> GParseState a
parseErrorInChildren child nrs message = GParseState (\path = GError (map (\nr = ([child nr:path],message)) nrs))

getCurrentPath :: GParseState GPath
getCurrentPath = GParseState (\path = GSuccess path)

withPath :: GPath (GParseState a) -> GParseState a
withPath path (GParseState f) = GParseState (\_ = f path)

runParse :: (GParseState a) -> GParseResult a
runParse (GParseState f) = f []

gToAModule :: !GModule !GinConfig !*World -> (GParseState AModule, *World)
gToAModule gmod =: { moduleKind = GCleanModule _ } config world
	= (parseError "Module does not have a graphical representation", world)
gToAModule gmod =: { moduleKind = GGraphicalModule definitions } config world
	# (res, world) = importModules config gmod.GModule.imports world
	| isError res = (parseError (fromError res), world)
	# imports = fromOk res
	# bindings = map getDefinitionBinding definitions
				 ++ (flatten o map getModuleBindings) imports
	= ( parseChildMap PNDefinition (gToADefinition bindings) definitions >>> \definitions = 
	    ret	{ AModule
			| name = gmod.GModule.name
			, types = gmod.GModule.types
			, imports = gmod.GModule.imports
			, definitions = definitions
			}
	  , world)

gToADefinition :: !Bindings !GDefinition -> GParseState ADefinition
gToADefinition bindings gdef
# graph = GGraphExpression (oryxDiagramToGraph bindings gdef.GDefinition.body)
= parseChild PNBody (gToAExpression bindings graph) >>> \body =
  ret { ADefinition 
      | name         = gdef.GDefinition.declaration.GDeclaration.name
      , formalParams = gdef.GDefinition.declaration.GDeclaration.formalParams
      , returnType   = gdef.GDefinition.declaration.GDeclaration.returnType
      , body         = body        
      , locals       = []
      }

gToAExpression :: !Bindings !GExpression -> GParseState (AExpression Void)
gToAExpression bindings (GUndefinedExpression) = parseError "Undefined expression"
gToAExpression bindings (GGraphExpression graph) = graphToSPTree bindings graph >>> spTreeToAExpression bindings
gToAExpression bindings (GListExpression gexps) =
    parseChildMap PNListItem (gToAExpression bindings) gexps >>> \aexps = 
    ret (List aexps)
gToAExpression bindings (GListComprehensionExpression glc) = 
    gToAListComprehension bindings glc >>> \alc = ret (ListComprehension alc)
gToAExpression bindings (GCleanExpression text) | size text == 0 = parseError "Clean expression is empty"
gToAExpression bindings (GCleanExpression text)                  = ret (Unparsed text)

gToAExpressionPath :: !Bindings !GExpression -> GParseState (AExpression Void)
gToAExpressionPath bindings exp = gToAExpression bindings exp >>> \exp` =
    getCurrentPath >>> \path =
    ret (PathContext path exp`)

gToAMaybeExpression :: !Bindings !(Maybe GExpression) -> GParseState (Maybe (AExpression Void))
gToAMaybeExpression bindings (Just exp) = gToAExpression bindings exp >>> \exp` = ret (Just exp`)
gToAMaybeExpression bindings Nothing = ret Nothing
    
gToAListComprehension :: !Bindings !GListComprehension -> GParseState (AListComprehension Void)
gToAListComprehension bindings glc = 
    gToAExpression bindings glc.GListComprehension.output >>> \output` =
    gToAExpression bindings glc.GListComprehension.input >>> \input` = 
    ret { output = output`
        , generators = ANestedGeneratorList [Generator glc.GListComprehension.selector input`]
        , guards = map Unparsed (maybeToList glc.GListComprehension.guard)
        }
        
spTreeToAExpression :: !Bindings !SPTree -> GParseState (AExpression Void)
spTreeToAExpression bindings (SPNode (SPPathNode node path)) = 
    withPath path (
        getNodeBinding node.GNode.name bindings >>> \nb = 
        case nb.NodeBinding.parameterMap of 
            NBPrefixApp = 
                if (isEmpty node.actualParams)
                    (ret (PathContext path (Var node.GNode.name)))
                    (parseChildMap PNActualParam (gToAExpressionPath bindings) node.GNode.actualParams >>> \exps =
                     getCurrentPath >>> \path` =
                     ret (PathContext path` (App [(Var node.GNode.name) : exps])))
            NBInfixApp _ _ = parseError "Infix node binding not implemented yet :-("
            NBCustom _ = parseError "Custom node binding not implemented yet :-("      
    )

spTreeToAExpression bindings (SPSeries first second pattern path) =
    spTreeToAExpression bindings first  >>> \first`  = 
    spTreeToAExpression bindings second >>> \second` =
    ret (PathContext path
			(case pattern of 
	    		Just p  = AppInfix ">>=" Infixl 1 first` (Lambda p second`)
		    	Nothing = AppInfix ">>|" Infixl 1 first` second`
		    )
		 )

spTreeToAExpression bindings (SPParallel (SPPathNode split splitPath, SPPathNode merge mergePath) branches) =
    withPath splitPath (
        getParallelBinding split.GNode.name merge.GNode.name bindings >>> \pb = 
        checkNrBranches pb (length branches) >>> \_ =     
        setParallelParams bindings (split,merge) branches pb.ParallelBinding.parameterMap >>> \exp = 
	    getCurrentPath >>> \path =
	    ret (PathContext path exp)
    )

checkNrBranches :: !ParallelBinding !Int -> GParseState Void
checkNrBranches pb i = case pb.fixedNrBranches of
    Just n | n == i = ret Void
    Just n          = parseError ("(" +++ pb.split.GDeclaration.name  +++ "," +++ pb.merge.GDeclaration.name 
                      +++ ") must have " +++ fromInt n +++ " branches")
    Nothing = ret Void

setParallelParams :: !Bindings !(GNode, GNode) ![(SPPattern,SPTree)] !(AExpression PBParameter) -> GParseState (AExpression Void)
setParallelParams _ _ _ (Unparsed s) = ret (Unparsed s)
setParallelParams _ _ _ (Lit s) = ret (Lit s)
setParallelParams _ _ _ (Var i) = ret (Var i)
setParallelParams bindings (split,merge) branches (App exps) =
	parseMap (setParallelParams bindings (split,merge) branches) exps >>> \exps` = 
    ret (App exps`)
setParallelParams bindings (split,merge) branches (AppInfix i fix pred exp1 exp2) =
	setParallelParams bindings (split,merge) branches exp1 >>> \exp1` =
	setParallelParams bindings (split,merge) branches exp2 >>> \exp2` =
    ret (AppInfix i fix pred exp1` exp2`)
    
setParallelParams bindings (split,merge) branches (Case exp casealts) =
	setParallelParams bindings (split,merge) branches exp >>> \exp` =
	parseMap (setParallelParamsCase bindings (split,merge) branches) casealts >>> \casealts` =
	ret (Case exp` casealts`)
setParallelParams bindings (split,merge) branches (Lambda pat exp) =
    setParallelParams bindings (split,merge) branches exp >>> \exp` =
    ret (Lambda pat exp`)
setParallelParams bindings (split,merge) branches (Tuple exps) = 
    parseMap (setParallelParams bindings (split,merge) branches) exps >>> \exps` =
        ret (Tuple exps`)
setParallelParams bindings (split,merge) branches (List exps) = 
    parseMap (setParallelParams bindings (split,merge) branches) exps >>> \exps` =
        ret (List exps`)
setParallelParams bindings (split,merge) branches (Extension ext) = case ext of
    PBSplitParameter i = gToAExpression bindings (split.GNode.actualParams !! i)
    PBMergeParameter i = gToAExpression bindings (merge.GNode.actualParams !! i)
    PBBranch i = spTreeToAExpression bindings (snd (branches !! i))
    PBBranchList = parseMap (spTreeToAExpression bindings) (map snd branches) >>> \branches` =
        ret (List branches`)
    PBApply f = parseMap (gToAExpression bindings) split.GNode.actualParams >>> \splitParams` =
    	parseMap (gToAExpression bindings) split.GNode.actualParams >>> \mergeParams` =
    	parseMap (spTreeToAExpression bindings) (map snd branches) >>> \branches` =
    	f splitParams` mergeParams` (zip2 (map fst branches) branches`)
    	
setParallelParamsCase :: !Bindings !(GNode, GNode) ![(SPPattern, SPTree)] !(ACaseAlt PBParameter) -> GParseState (ACaseAlt Void)
setParallelParamsCase bindings (split,merge) branches (CaseAlt pat exp) =
	setParallelParams bindings (split,merge) branches exp >>> \exp` =
	ret (CaseAlt pat exp`)	
    	
foldl1 :: (a a -> a) [a] -> a
foldl1 f [x:xs]  =  foldl f x xs

