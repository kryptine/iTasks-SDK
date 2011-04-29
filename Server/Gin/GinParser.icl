implementation module GinParser

import StdBool
import StdArray
import StdEnum
from StdFunc import o,flip
import StdMisc
import StdList
import StdOrdList
import StdTuple

import GenPrint

//Clean-platform:
import Graph
import Map
import Void
import JSON
import Text

//GiN:
import Monad
import GinSyntax
import GinAbstractSyntax
import GinFlowLibrary
import GinORYX
import GinStorage

//--------------------------------------------------------------------------------------------------
//GParseState monadic combinators

instance toString GPath where
	toString nodes = "/" +++ (((foldr (\a b -> a +++ "/" +++ b) "") o reverse o map toString) nodes)

instance toString GPathNode where
    toString (PNDefinition i)	= toS "definitions" i
    toString PNBody				= "body"
    toString (PNNode i)			= toS "nodes" i
    toString (PNEdge i)			= toS "edges" i
    toString (PNActualParam i)	= toS "actualParameters" i
    toString PNPattern			= "pattern"
    toString (PNListItem i)		= toS "items" i

toS :: !String Int -> String
toS name curNode= name +++ "[" +++ toString curNode+++ "]"
    
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

//-------------------------------------------------------------------------------------------
//Top-level conversion function

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
      
gToAExpression :: !Bindings !GExpression -> GParseState (AExpression a)
gToAExpression bindings (GUndefinedExpression) = parseError "Undefined expression"
gToAExpression bindings (GGraphExpression graph) = graphToAExpression bindings graph
gToAExpression bindings (GListExpression gexps) =
    parseChildMap PNListItem (gToAExpression bindings) gexps >>> \aexps = 
    ret (List aexps)
gToAExpression bindings (GListComprehensionExpression glc) = 
    gToAListComprehension bindings glc >>> \alc = ret (ListComprehension alc)
gToAExpression bindings (GCleanExpression text) | size text == 0 = parseError "Clean expression is empty"
gToAExpression bindings (GCleanExpression text)                  = ret (Unparsed text)

gToAExpressionPath :: !Bindings !GExpression -> GParseState (AExpression a)
gToAExpressionPath bindings exp = gToAExpression bindings exp >>> \exp` =
    getCurrentPath >>> \path =
    ret (PathContext path exp`)

gToAMaybeExpression :: !Bindings !(Maybe GExpression) -> GParseState (Maybe (AExpression a))
gToAMaybeExpression bindings (Just exp) = gToAExpression bindings exp >>> \exp` = ret (Just exp`)
gToAMaybeExpression bindings Nothing = ret Nothing
    
gToAListComprehension :: !Bindings !GListComprehension -> GParseState (AListComprehension a)
gToAListComprehension bindings glc = 
    gToAExpression bindings glc.GListComprehension.output >>> \output` =
    gToAExpression bindings glc.GListComprehension.input >>> \input` = 
    ret { output = output`
        , generators = ANestedGeneratorList [Generator glc.GListComprehension.selector input`]
        , guards = map Unparsed (maybeToList glc.GListComprehension.guard)
        }

//--------------------------------------------------------------------------------------------------------------
//GGraph decomposition

graphToAExpression :: !Bindings !GGraph -> GParseState (AExpression a)
graphToAExpression bindings graph =
	toTreeGraph graph >>> \tg ->
	ret (decompose bindings (tg)) >>> \tg ->
	//trace_n ("after decomp:" +++ showTreeGraph tg) (ret tg) >>> \tg -> 
	treeGraphToAExpression bindings tg

:: TreeGraph = TreeGraph (Graph TreeGraphNode GEdge) NodeIndex NodeIndex
:: TreeGraphNode = TGNode GNode | TGSubgraph TreeGraph

isNode :: TreeGraphNode -> Bool
isNode (TGNode _) = True
isNode _          = False

isSubgraph :: TreeGraphNode -> Bool
isSubgraph (TGSubgraph _) = True
isSubgraph _             = False

fromNode :: TreeGraphNode -> GNode
fromNode (TGNode n) = n

fromSubgraph :: TreeGraphNode -> TreeGraph
fromSubgraph (TGSubgraph t) = t

toTreeGraph :: GGraph -> GParseState TreeGraph
toTreeGraph (GGraph graph)
# mbSource = sourceNode graph
  mbSink   = sinkNode graph
| isNothing mbSource = parseError "No unique source node"
| isNothing mbSink = parseError "No unique sink node"
= ret (TreeGraph (mapNodes (\n -> TGNode n) graph) (fromJust mbSource) (fromJust mbSink))

edgePairs :: (Graph n e) EdgeIndex -> [(EdgeIndex,EdgeIndex)]
edgePairs graph startEdge = (diag2 (successorEdges startEdge graph) (predecessorEdges startEdge graph))

decompose :: Bindings TreeGraph -> TreeGraph
decompose bindings tg=:(TreeGraph g source sink)
	| nodeCount g == 1 = tg //Trivial graph: No decomposition possible
	# loop = (sink,source) 
	# pairs = edgePairs g loop
	= decomp (edgePairs g loop) loop (TreeGraph (addEdge Nothing loop g) source sink)
where
	decomp :: [(EdgeIndex,EdgeIndex)] EdgeIndex TreeGraph -> TreeGraph
	decomp [] loop tg = tg
	decomp [(fromEdge,toEdge):edges] loop tg=:(TreeGraph g source sink)
		| not (   isParallelBinding (nodename (fst fromEdge)) (nodename (snd toEdge)) bindings
			   || isParallelBinding (nodename (snd fromEdge)) (nodename (fst toEdge)) bindings
			  ) = decomp edges loop tg
		# comps = components (removeEdge fromEdge (removeEdge toEdge g))
		| isEmpty (tl comps) = decomp edges loop tg //try next pair
		# [g,s:_] = comps
		# (g,s) = if (nodeExists source g) (g,s) (s,g)
		# g = removeEdge loop g
		# (newNode,g) = addNode (TGSubgraph(decompose bindings (TreeGraph s (snd fromEdge) (fst toEdge)))) g //decompose subgraph s recursively
		# g = addEdge Nothing (fst fromEdge,newNode) g
		# g = addEdge Nothing (newNode, snd toEdge) g
		# source = if (source == snd fromEdge) newNode source
		# sink = if (sink == fst toEdge) newNode sink
		= (TreeGraph g source sink)
	where
		nodename :: NodeIndex -> String
		nodename nodeIndex = case fromJust (getNodeData nodeIndex g) of
			TGNode    { GNode | name } = name
			TGSubgraph _                = ""

treeGraphNodeToAExpression :: !Bindings !TreeGraphNode -> GParseState (AExpression a)
treeGraphNodeToAExpression bindings (TGNode node) = nodeToAExpression bindings node
treeGraphNodeToAExpression bindings (TGSubgraph graph) = treeGraphToAExpression bindings graph

treeGraphToAExpression :: !Bindings !TreeGraph -> GParseState (AExpression a)
treeGraphToAExpression bindings tg=:(TreeGraph graph source sink)
	| isEmptyGraph graph = parseError "Empty graph"
	//Trivial subgraphs
	| isTrivialGraph graph
		= treeGraphNodeToAExpression bindings (fromJust (getNodeData (hd (nodeIndices graph)) graph))
	//Structured parallel subgraphs
	# mbParallel = parallelDecompose bindings tg
	| isJust mbParallel = parallelToAExpression bindings (fromJust mbParallel)
	//Sequential subgraphs
	| isSequential tg = sequenceToAExpression bindings tg
	//Other graphs
	= parseError "Mapping not supported"

nodeToAExpression :: !Bindings !GNode -> GParseState (AExpression a)
nodeToAExpression bindings node =
    getNodeBinding node.GNode.name bindings >>> \nb = 
    case nb.NodeBinding.parameterMap of 
        NBPrefixApp = 
            if (isEmpty node.actualParams)
                (ret (Var node.GNode.name))
                (parseChildMap PNActualParam (gToAExpressionPath bindings) node.GNode.actualParams >>> \exps =
                 ret (App [(Var node.GNode.name) : exps]))
        NBInfixApp _ _ = parseError "Infix node binding not implemented yet"
        NBApply _ = parseError "Custom node binding not implemented yet"      

isSequential :: TreeGraph -> Bool
isSequential graph = True //TODO: check if graph does not contain any parallel split or parallel merge node.

parallelDecompose :: !Bindings !TreeGraph -> Maybe (GNode, GNode, [(GEdge, TreeGraphNode)])
parallelDecompose bindings (TreeGraph graph sourceIndex sinkIndex)
	# branchIndices = directSuccessors sourceIndex graph
	| sort branchIndices <> sort (directPredecessors sinkIndex graph) = Nothing
	= case (getNodeData sourceIndex graph, getNodeData sinkIndex graph) of
		(Just (TGNode source), Just (TGNode sink)) = 
			Just (source, sink, [ (fromJust (getEdgeData (sourceIndex,n) graph), fromJust (getNodeData n graph)) \\ n <- branchIndices])
		_ = Nothing

parallelToAExpression :: !Bindings (GNode, GNode, [(GEdge, TreeGraphNode)]) -> GParseState (AExpression a)
parallelToAExpression bindings (split, merge, branches) =
//    withPath splitPath (
        getParallelBinding split.GNode.name merge.GNode.name bindings >>> \pb = 
        checkNrBranches pb (length branches) >>> \_ =     
        setParallelParams bindings (split,merge) branches pb.ParallelBinding.parameterMap >>> \exp = 
	    getCurrentPath >>> \path =
	    ret (PathContext path exp)
//    )

checkNrBranches :: !ParallelBinding !Int -> GParseState Void
checkNrBranches pb i = case pb.fixedNrBranches of
    Just n | n == i = ret Void
    Just n          = parseError ("(" +++ pb.split.GDeclaration.name  +++ "," +++ pb.merge.GDeclaration.name 
                      +++ ") must have " +++ fromInt n +++ " branches")
    Nothing = ret Void

setParallelParams :: !Bindings !(GNode, GNode) ![(GEdge, TreeGraphNode)] !(AExpression PBParameter) -> GParseState (AExpression a)
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
    PBBranch i = treeGraphNodeToAExpression bindings (snd (branches !! i))
    PBBranchList = parseMap (treeGraphNodeToAExpression bindings) (map snd branches) >>> \branches` =
        ret (List branches`)

setParallelParamsCase :: !Bindings !(GNode, GNode) ![(GEdge, TreeGraphNode)] !(ACaseAlt PBParameter) -> GParseState (ACaseAlt a)
setParallelParamsCase bindings (split,merge) branches (CaseAlt pat exp) =
	setParallelParams bindings (split,merge) branches exp >>> \exp` =
	ret (CaseAlt pat exp`)

sequenceToAExpression :: !Bindings !TreeGraph -> GParseState (AExpression a) 	
sequenceToAExpression bindings (TreeGraph graph source sink)
	# mergeNodes = filter (\n -> nodeName n graph == Just "case merge" 
	                       && not (isPathToMergeSink n sink)) (nodeIndices graph)
	
	= seqNodeToAExpr source sink mergeNodes >>> \sourceExp ->
	  parseMap (\i -> seqNodeToAExpr (hd (directSuccessors i graph)) sink mergeNodes >>> \exp -> ret (i,exp)) 
	           mergeNodes >>> \letExps ->
   
	 let variableMap = collectVariableExps letExps
	 in ret (	if (isEmpty letExps)
				(setParams variableMap sourceExp)
			 	(Let	(map (\(i,exp) -> ("merge" +++ toString i +++ " " +++ unwords (getNodeVariables i variableMap), setParams variableMap exp)) letExps) 
						(setParams variableMap sourceExp)
				)
			)
where
	isPathToMergeSink :: NodeIndex NodeIndex -> Bool
	isPathToMergeSink current sink
		# nodeData = fromJust (getNodeData current graph)
		| isSubgraph nodeData = False
		# node = fromNode nodeData
		| node.GNode.name <> "case merge" = False
		| current == sink = True
		= case directSuccessors current graph of
			[n] = isPathToMergeSink n sink
			_   = False
			
	seqNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] -> GParseState (AExpression NodeIndex)
	seqNodeToAExpr current sink mergeNodes
		# nodeData = (fromJust (getNodeData current graph))
		| current == sink = treeGraphNodeToAExpression bindings nodeData                    //1
		| isNode nodeData
			# node = fromNode nodeData
			# nodename = node.GNode.name
			| isMember current mergeNodes = ret (Extension current)                         //2: recursive call
			| nodename == "case split" = caseNodeToAExpr current sink mergeNodes            //3
			| nodename == "let" = letNodeToAExpr current sink mergeNodes                    //4
			= seqNode` nodeData 
		= seqNode` nodeData
	where
		seqNode` nodeData
			# successors = directSuccessors current graph
			| not (isEmpty (tl successors)) = parseError "Node cannot be used as split node"    //7
			# successor = hd successors
			| isPathToMergeSink successor sink = seqNodeToAExpr current current mergeNodes
			# edgePattern = fromJust (getEdgeData (current, successor) graph)                   //5,6
		    = treeGraphNodeToAExpression bindings nodeData >>> \first`  -> 
		      seqNodeToAExpr successor sink mergeNodes     >>> \second` ->
			    ret (case edgePattern of 
		    			Just p  = (AppInfix ">>=" Infixl 1 first` (Lambda p second`))
				    	Nothing = (AppInfix ">>|" Infixl 1 first` second`)
			    	)

	caseNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] -> GParseState (AExpression NodeIndex)
	caseNodeToAExpr current sink mergeNodes
		# nodeData = (fromJust (getNodeData current graph))
		# node = fromNode nodeData
		# successors = directSuccessors current graph
		| isEmpty successors = parseError "Missing node(s) after case split"
		| any (flip isPathToMergeSink sink) successors = parseError "Missing task in branch"
		# caseExpression = node.GNode.actualParams
		= gToAExpression bindings (node.GNode.actualParams !! 0) >>> \caseExpr -> 
		  parseMap (\n -> seqNodeToAExpr n sink mergeNodes >>> \exp -> 
		            ret (fromJust (getEdgeData (current,n) graph), exp)) successors >>> \caseAlts ->
		  if (length (filter isNothing (map fst caseAlts)) > 1)
			(parseError "A case expression can have at most one default case")
			//"otherwise" alternative is put at the end of the list
			(ret (let sortalts = filter (not o isEmptyEdge o fst) caseAlts ++ filter (isEmptyEdge o fst) caseAlts
			    in Case caseExpr (map mkCaseAlt caseAlts)))
	where
		isEmptyEdge :: GEdge -> Bool
		isEmptyEdge Nothing = True
		isEmptyEdge (Just pat) = size (trim pat) == 0
	
		mkCaseAlt :: (GEdge, AExpression NodeIndex) -> ACaseAlt NodeIndex
		mkCaseAlt (Just pat, exp) = CaseAlt pat exp
		mkCaseAlt (Nothing, exp)  = CaseAlt "otherwise" exp
		  
	letNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] -> GParseState (AExpression NodeIndex)
	letNodeToAExpr current sink mergeNodes 
		# nodeData = (fromJust (getNodeData current graph))
		# node = fromNode nodeData
		# successors = directSuccessors current graph
		| isEmpty successors = parseError "Missing node after let"
		| not (isEmpty (tl successors)) = parseError "Let cannot be used as split node"
		| isPathToMergeSink (hd successors) sink = parseError "Missing task in branch"
		= getNodeParameter node 0 >>> \pat ->
		  gToAExpression bindings (node.GNode.actualParams !! 1) >>> \exp -> 
		  seqNodeToAExpr (hd successors) sink mergeNodes >>> \inExpr ->
		  ret (Let [(pat,exp)] inExpr)

	getNodeParameter :: GNode Int -> GParseState String
	getNodeParameter { actualParams } index = getTextExpr (actualParams !! index)
	where
		getTextExpr :: GExpression -> GParseState String
		getTextExpr (GCleanExpression exp) = ret exp
		getTextExpr _ = parseError "Expected: textual parameter"

nodeName nodeIndex graph = case fromJust (getNodeData nodeIndex graph) of
	TGNode{ GNode | name } = Just name
	_                     = Nothing

//--------------------------------------------------------------------------------------------------
//Determine arguments of tail-recursive calls

:: Scope :== [GIdentifier]

bind :: !GIdentifier !Scope -> Scope
bind i scope = [i : filter (\p = p <> i) scope]

bindPattern :: !GPattern Scope -> Scope
bindPattern pattern scope = foldr bind scope (extractVariables pattern)

:: NodeVariables = 
	{ freeVariables		:: [GIdentifier] // free variables in expression (free variables in all successors should be added as well...)
	, boundVariables	:: [GIdentifier] // union of bound variables of all *incoming* branches
	}
:: NodeVariableMap :== Map NodeIndex NodeVariables

getNodeVariables :: NodeIndex NodeVariableMap -> [GIdentifier]
getNodeVariables nodeIndex nvMap
	= case get nodeIndex nvMap of
		Nothing = []
		Just nv = sort [ v \\ v <- nv.freeVariables | isMember v nv.boundVariables ]

updateNodeVariableMap :: NodeIndex (NodeVariables -> NodeVariables) NodeVariableMap -> NodeVariableMap
updateNodeVariableMap index f nvMap
	# nv = case get index nvMap of 
		Nothing = { NodeVariables | freeVariables = [], boundVariables = [] }
		Just nv = nv
	= put index (f nv) nvMap

addFreeVariables :: NodeIndex [GIdentifier] NodeVariableMap -> NodeVariableMap
addFreeVariables nodeIndex vars nvMap = updateNodeVariableMap nodeIndex (\nv -> { nv & freeVariables = foldr bind nv.freeVariables vars }) nvMap

addBoundVariables :: NodeIndex [GIdentifier] NodeVariableMap -> NodeVariableMap
addBoundVariables nodeIndex vars nvMap = updateNodeVariableMap nodeIndex (\nv -> { nv & boundVariables = foldr bind nv.boundVariables vars }) nvMap

collectVariableExps ::  [(NodeIndex, AExpression NodeIndex)] -> NodeVariableMap
collectVariableExps [] = newMap
collectVariableExps [(index, exp): exps] = collectVariables index [] (collectVariableExps exps) exp

collectVariables :: NodeIndex Scope NodeVariableMap (AExpression NodeIndex) -> NodeVariableMap
collectVariables curNode scope acc (Unparsed s) = addFreeVariables curNode (freeVariablesInCleanExpr scope s) acc
collectVariables curNode scope acc (Lit s) = acc
collectVariables curNode scope acc (Var v) = if (isMember v scope) acc (addFreeVariables curNode [v] acc)
collectVariables curNode scope acc (App exps) 
	= collectMap curNode scope acc collectVariables exps
collectVariables curNode scope acc (AppInfix i fix prec e1 e2) = 
	collectVariables curNode scope (collectVariables curNode scope acc e1) e2
collectVariables curNode scope acc (Lambda pat exp) 
	= collectVariables curNode (bindPattern pat scope) acc exp
collectVariables curNode scope acc (Let defs exp)
	# scope = foldr bindPattern scope (map fst defs)
	= collectVariables curNode scope (collectMap curNode scope acc collectLetDefs defs) exp
collectVariables curNode scope acc (Case exp alts)
	= collectMap curNode scope (collectVariables curNode scope acc exp) collectCaseAlt alts
collectVariables curNode scope acc (Tuple exps) 
	= collectMap curNode scope acc collectVariables exps
collectVariables curNode scope acc (List exps)
	= collectMap curNode scope acc collectVariables exps
//collectVariables curNode scope acc (ListComprehension alc)
//	= collectListComprehension curNode scope acc alc
collectVariables curNode scope acc (PathContext path exp) = collectVariables curNode scope acc exp
collectVariables curNode scope acc (Extension toNode) = addBoundVariables toNode scope acc

collectLetDefs :: NodeIndex Scope NodeVariableMap (APattern, AExpression NodeIndex) -> NodeVariableMap
collectLetDefs curNode scope acc (pat, exp) = collectVariables curNode scope acc exp //!!

collectCaseAlt :: NodeIndex Scope NodeVariableMap (ACaseAlt NodeIndex) -> NodeVariableMap
collectCaseAlt curNode scope acc (CaseAlt pat exp)
	# scope` = if (trim pat == "_") scope (bindPattern pat scope)
	= collectVariables curNode scope` acc exp

collectMap :: NodeIndex Scope NodeVariableMap (NodeIndex Scope NodeVariableMap a -> NodeVariableMap) [a] -> NodeVariableMap
collectMap curNode scope acc f []     = acc
collectMap curNode scope acc f [x:xs] = collectMap curNode scope (f curNode scope acc x) f xs

freeVariablesInCleanExpr :: Scope !String -> [GIdentifier]
freeVariablesInCleanExpr scope expr = filter (\v -> not (isMember v scope)) (extractVariables expr)

extractVariables :: !String -> [String] //TODO: Replace by a real parser
extractVariables s = filter isVariable (splitOn (not o variableChar) s)
where
	splitOn :: !(Char -> Bool) !String -> [String]
	splitOn pred s = split s 0
	where
		split :: !String !Int -> [String]
		split s i | i == size s = [s]
		split s i | pred s.[i] = [ s % (0,i - 1) : split (s % (i + 1, size s)) 0 ]
		                       = split s (i+1)

	variableChar :: Char -> Bool
	variableChar c = isAlphanum c || isMember c ['_', '`']
	
	isVariable :: !String -> Bool
	isVariable "" = False
	isVariable s = isVar 0 where
		isVar :: Int -> Bool
		isVar i | i == size s = isLower s.[0]
	                          = variableChar s.[i] && isVar (i+1)

setParams :: NodeVariableMap (AExpression NodeIndex) -> AExpression a
setParams nvMap (Unparsed s) = Unparsed s
setParams nvMap (Lit s) = Lit s
setParams nvMap (Var v) = Var v
setParams nvMap (App exps) 
	= App (setMap nvMap setParams exps)
setParams nvMap (AppInfix i fix prec e1 e2) = 
	AppInfix i fix prec (setParams nvMap e1) (setParams nvMap e2)
setParams nvMap (Lambda pat exp)
	= Lambda pat (setParams nvMap exp)
setParams nvMap (Let defs exp)
	= Let (setMap nvMap setLetDefsParams defs) (setParams nvMap exp)
setParams nvMap (Case exp alts)
	= Case (setParams nvMap exp) (setMap nvMap setCaseAltParams alts)
setParams nvMap (Tuple exps) 
	= Tuple (setMap nvMap setParams exps)
setParams nvMap (List exps)
	= List (setMap nvMap setParams exps)
//setParams nvMap (ListComprehension alc)
//	= collectListComprehension nvMap alc
setParams nvMap (PathContext path exp) = setParams nvMap exp
setParams nvMap (Extension toNode)
	# fun = Var ("merge" +++ toString toNode)
	# args = getNodeVariables toNode nvMap
	| isEmpty args = fun
	= App [fun : map Var args]

setLetDefsParams :: NodeVariableMap (APattern, AExpression NodeIndex) -> (APattern, AExpression a)
setLetDefsParams nvMap (pat, exp) = (pat, setParams nvMap exp)

setCaseAltParams :: NodeVariableMap (ACaseAlt NodeIndex) -> (ACaseAlt a)
setCaseAltParams nvMap (CaseAlt pat exp) = CaseAlt pat (setParams nvMap exp)

setMap :: NodeVariableMap (NodeVariableMap a -> b) [a] -> [b]
setMap nvMap f []     = []
setMap nvMap f [x:xs] = [f nvMap x: setMap nvMap f xs]

//--------------------------------------------------------------------------------------------------
// for debugging

/*
derive gPrint (,)
derive gPrint NodeVariables

printNodeVariableMap :: NodeVariableMap -> String
printNodeVariableMap nvMap = printToString (toList nvMap)
showTreeGraph :: TreeGraph -> String
showTreeGraph (TreeGraph graph source sink) = "[" +++ 
	toString (length (nodeIndices graph)) +++ " nodes, " +++ toString (length (edgeIndices graph)) +++ " edges, source=" +++ 
		toString source +++ ", sink=" +++ toString sink +++ ". " +++
	showTreeNodes [ (n,fromJust (getNodeData n graph)) \\ n <- nodeIndices graph ]
					+++ showEdges (edgeIndices graph) +++ "]"								
where
	showTreeNodes :: [(NodeIndex,TreeGraphNode)] -> String
	showTreeNodes [] = ""
	showTreeNodes [(i,TGNoden):ns] = toString i +++ "=" +++ n.GNode.name +++ "," +++ showTreeNodes ns
	showTreeNodes [(i,TGSubgraphtg):ns] = toString i +++ "=(" +++ showTreeGraph tg +++ ")" +++"," +++ showTreeNodes ns
	
	showEdges :: [EdgeIndex] -> String
	showEdges [] = ""
	showEdges [(fromNode,toNode):edges] = "(" +++ toString fromNode +++ "," +++ toString toNode +++ ")," +++ showEdges edges
*/

//--------------------------------------------------------------------------------------------------
//Utilities
    	
foldl1 :: (a a -> a) [a] -> a
foldl1 f [x:xs]  =  foldl f x xs

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x
unwords [x:xs] = x +++ " " +++ unwords xs
