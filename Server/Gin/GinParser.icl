implementation module GinParser

import StdBool
import StdArray
import StdEnum
from StdFunc import o,flip,seq
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
          
instance PathNode GNode
where
	getPathNode :: GNode GPath -> GPath
	getPathNode { GNode | identifier } _ = [NodePath identifier]

instance PathNode GEdge
where
	getPathNode :: GEdge GPath -> GPath
	getPathNode { GEdge | identifier} _ = [EdgePath identifier]
	
instance PathNode (Int,GExpression)
where
	getPathNode :: (Int,GExpression) GPath -> GPath
	getPathNode (i,_) path = [ParamPath i : path]
	
instance PathNode TreeGraph
where
	getPathNode :: TreeGraph GPath -> GPath
	getPathNode (TreeGraph graph source sink) path = getPathNode (fromJust (getNodeData source graph)) path

instance PathNode TreeGraphNode
where
	getPathNode :: TreeGraphNode GPath -> GPath
	getPathNode (TGNode node)         path = getPathNode node path 
	getPathNode (TGSubgraph subgraph) path = getPathNode subgraph path
	
parseChild :: a (GParseState b) -> GParseState b| PathNode a
parseChild child (GParseState f) = GParseState (\path = f (getPathNode child path))

parseMap :: (a -> GParseState b) [a] -> GParseState [b]
parseMap _ []     = ret []
parseMap f [x:xs] = f x >>> \x` = 
				                  parseMap f xs >>> \xs` = 
                                  ret [x`:xs`]

parseChildMap :: (a -> GParseState b) [a] -> GParseState [b]| PathNode a
parseChildMap f [] = ret []
parseChildMap f [x:xs] = parseChild x (f x) >>> \x` ->
						 parseChildMap f xs >>> \xs` ->
						 ret [x`:xs`]

orElse :: (GParseState a) (GParseState a) -> GParseState a
orElse (GParseState m) (GParseState k) = 
	GParseState (\p = case (m p) of
		GSuccess a    = GSuccess a
		GError errors = k p)

parseError :: String -> GParseState a
parseError message = GParseState (\path = GError [(path,message)])

parseErrorInChild :: a String -> GParseState b| PathNode a
parseErrorInChild child message = parseChild child (parseError message)

parseErrorInChildren :: [a] String -> GParseState b| PathNode a
parseErrorInChildren children message = GParseState (\path = GError [(getPathNode child path, message) \\ child <- children])

getCurrentPath :: GParseState GPath
getCurrentPath = GParseState (\path = GSuccess path)

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
	= ( parseMap (gToADefinition bindings) definitions >>> \definitions = 
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
= gToAExpression bindings graph >>> \body =
  ret { ADefinition 
      | name         = gdef.GDefinition.declaration.GDeclaration.name
      , formalParams = gdef.GDefinition.declaration.GDeclaration.formalParams
      , returnType   = gdef.GDefinition.declaration.GDeclaration.returnType
      , body         = body        
      , locals       = []
      }
      
gToAExpression :: !Bindings !GExpression -> GParseState (AExpression Void)
gToAExpression bindings (GUndefinedExpression) = parseError "Undefined expression"
gToAExpression bindings (GGraphExpression graph) = graphToAExpression bindings graph
gToAExpression bindings (GListExpression gexps) =
    parseMap (gToAExpression bindings) gexps >>> \aexps = 
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
        , generators = NestedGeneratorList [Generator glc.GListComprehension.selector input`]
        , guards = map Unparsed (maybeToList glc.GListComprehension.guard)
        }

//--------------------------------------------------------------------------------------------------------------
//GGraph decomposition

graphToAExpression :: !Bindings !GGraph -> GParseState (AExpression Void)
graphToAExpression bindings graph =
	toTreeGraph graph >>> \tg ->
	ret (decompose bindings (tg)) >>> \tg ->
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
# sources = filterNodes (\pred _ _ -> isEmpty pred) graph 
| isEmpty sources = parseError "No source node found"
| (not o isEmpty) (tl sources) = parseErrorInChildren (getNodes sources graph) "Source node is not unique" 
# sinks = filterNodes (\_ succ _ -> isEmpty succ) graph 
| isEmpty sinks = parseError "No sink node found"
| (not o isEmpty) (tl sinks) = parseErrorInChildren (getNodes sinks graph) "Sink node is not unique" 
= ret (TreeGraph (mapNodes (\n -> TGNode n) graph) (hd sources) (hd sinks))

edgePairs :: (Graph n e) EdgeIndex -> [(EdgeIndex,EdgeIndex)]
edgePairs graph startEdge = (diag2 (successorEdges startEdge graph) (predecessorEdges startEdge graph))

decompose :: Bindings TreeGraph -> TreeGraph
decompose bindings tg=:(TreeGraph g source sink)
	| nodeCount g == 1 = tg //Trivial graph: No decomposition possible
	# loop = (sink,source) 
	# pairs = edgePairs g loop
	= decomp (edgePairs g loop) loop (TreeGraph (addEdge emptyEdge loop g) source sink)
where
	decomp :: [(EdgeIndex,EdgeIndex)] EdgeIndex TreeGraph -> TreeGraph
	decomp [] loop (TreeGraph g source sink) = TreeGraph (removeEdge loop g) source sink
	decomp [(fromEdge,toEdge):edges] loop tg=:(TreeGraph g source sink)
		| not (   isParallelBinding (nodeName (fst fromEdge) g) (nodeName (snd toEdge) g) bindings
			   || isParallelBinding (nodeName (snd fromEdge) g) (nodeName (fst toEdge) g) bindings
			  ) = decomp edges loop tg
		# comps = components (removeEdge fromEdge (removeEdge toEdge g))
		| isEmpty (tl comps) = decomp edges loop tg //try next pair
		# [g,s:_] = comps
		# (g,s) = if (nodeExists source g) (g,s) (s,g)
		| isTrivialGraph s = decomp edges loop tg
		# g = removeEdge loop g
		# (newNode,g) = addNode (TGSubgraph(decompose bindings (TreeGraph s (snd fromEdge) (fst toEdge)))) g //decompose subgraph s recursively
		# g = addEdge emptyEdge (fst fromEdge,newNode) g
		# g = addEdge emptyEdge (newNode, snd toEdge) g
		# source = if (source == snd fromEdge) newNode source
		# sink = if (sink == fst toEdge) newNode sink
		= decompose bindings (TreeGraph g source sink)

treeGraphNodeToAExpression :: !Bindings !TreeGraphNode -> GParseState (AExpression Void)
treeGraphNodeToAExpression bindings (TGNode node) = nodeToAExpression bindings node
treeGraphNodeToAExpression bindings (TGSubgraph graph) = treeGraphToAExpression bindings graph

treeGraphToAExpression :: !Bindings !TreeGraph -> GParseState (AExpression Void)
treeGraphToAExpression bindings tg=:(TreeGraph graph source sink)
	| isEmptyGraph graph = parseErrorInChild tg "Empty graph"
	//Trivial subgraphs
	| isTrivialGraph graph
		= treeGraphNodeToAExpression bindings (fromJust (getNodeData (hd (nodeIndices graph)) graph))
	//Structured parallel subgraphs
	# mbParallel = parallelDecompose bindings tg
	| isJust mbParallel = parallelToAExpression bindings (fromJust mbParallel)
	//Sequential subgraphs
	| isSequential tg = sequenceToAExpression bindings tg
	//Other graphs
	= parseErrorInChild tg "Mapping not supported"

nodeToAExpression :: !Bindings !GNode -> GParseState (AExpression Void)
nodeToAExpression bindings node = 
	parseChild node
	    ( getNodeBinding node.GNode.name bindings >>> \nb = 
	    case nb.NodeBinding.parameterMap of 
	        NBBuiltIn = parseError "Node not allowed here"
	        NBPrefixApp = 
	            if (isEmpty node.GNode.actualParams)
	                (ret (Var node.GNode.name))
	                (parseChildMap (gToAExpressionPath bindings o snd) (zip2 [0..] node.GNode.actualParams) >>> \exps =
	                 ret (App [(Var node.GNode.name) : exps]))
		)

//-------------------------------------------------------------------------------------------
//Mapping of parallel subgraphs

parallelDecompose :: !Bindings !TreeGraph -> Maybe (GNode, GNode, [TreeGraphNode])
parallelDecompose bindings (TreeGraph graph sourceIndex sinkIndex)
	# branchIndices = directSuccessors sourceIndex graph
	| sort branchIndices <> sort (directPredecessors sinkIndex graph) = Nothing
	| not (isParallelBinding (nodeName sourceIndex graph) (nodeName sinkIndex graph) bindings) = Nothing
	# source = fromNode (fromJust (getNodeData sourceIndex graph))
	# sink = fromNode (fromJust (getNodeData sinkIndex graph))
	= Just (source, sink, [ fromJust (getNodeData n graph) \\ n <- branchIndices])

parallelToAExpression :: !Bindings (GNode, GNode, [TreeGraphNode]) -> GParseState (AExpression Void)
parallelToAExpression bindings (split, merge, branches) =
    parseChild split (
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

setParallelParams :: !Bindings !(GNode, GNode) ![TreeGraphNode] !(AExpression PBParameter) -> GParseState (AExpression Void)
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
    PBBranch i = 
    	case branches of
    		[TGNode node] = if (node.GNode.name == "list comprehension")
    			(parseError "List comprehension not allowed here")
    			(nodeToAExpression bindings node)
    		_	= (treeGraphNodeToAExpression bindings (branches !! i))
	PBBranchList = 
		case branches of
			[TGNode node]
				= if (node.GNode.name == "list comprehension") (setListComprehension node) setList
			_	= setList
		where
		setListComprehension node = 
			getNodePattern node 0 >>> \generatorPattern -> 
			gToAExpression bindings (node.GNode.actualParams !! 1) >>> \generatorExpression ->
			getNodePattern node 2 >>> \guard ->
			gToAExpression bindings (node.GNode.actualParams !! 3) >>> \output ->
			ret
				( ListComprehension 
					{ AListComprehension
					| output = output
					, generators = NestedGeneratorList [Generator generatorPattern generatorExpression]
					, guards = if (trim guard == "") [] [Unparsed guard]
					}
				)
		setList = parseMap (treeGraphNodeToAExpression bindings) branches >>> \branches` ->
     				      ret (List branches`)

setParallelParamsCase :: !Bindings !(GNode, GNode) ![TreeGraphNode] !(ACaseAlt PBParameter) -> GParseState (ACaseAlt Void)
setParallelParamsCase bindings (split,merge) branches (CaseAlt pat exp) =
	setParallelParams bindings (split,merge) branches exp >>> \exp` =
	ret (CaseAlt pat exp`)

//-------------------------------------------------------------------------------------------
//Mapping of sequential subgraphs

isSequential :: TreeGraph -> Bool
isSequential graph = True //TODO: check if graph does not contain any parallel split or parallel merge node.

:: Scope :== [GIdentifier]

bindVar :: !GIdentifier !Scope -> Scope
bindVar var scope = [var : filter (\p = p <> var) scope]

bindVars :: ![GIdentifier] !Scope -> Scope
bindVars vars scope = foldr bindVar scope vars

:: ScopeMap :== Map NodeIndex Scope
updateScopeMap :: ![NodeIndex] ![GIdentifier] !ScopeMap -> ScopeMap
updateScopeMap [] _ scopeMap = scopeMap
updateScopeMap [n:ns] vars scopeMap
	= updateScopeMap ns vars (put n (bindVars vars (fromMaybe [] (get n scopeMap))) scopeMap)

getScope :: !NodeIndex !ScopeMap -> [GIdentifier]
getScope n scopeMap = fromMaybe [] (get n scopeMap)

sequenceToAExpression :: !Bindings !TreeGraph -> GParseState (AExpression Void) 	
sequenceToAExpression bindings (TreeGraph graph source sink)
	# (updateMap, graph) = mapIndicesReversePostorder graph source
	# source = fromJust (get source updateMap)
	# sink = fromJust (get sink updateMap)
	# tg = (TreeGraph graph source sink)
	= parseChild tg (sequenceToAExpression` bindings tg)

sequenceToAExpression` :: !Bindings !TreeGraph -> GParseState (AExpression Void)
sequenceToAExpression` bindings (TreeGraph graph source sink)
	# mergeNodes = filter (\n -> nodeName n graph == "case merge"
	                       && not (isPathToMergeSink n sink)) (nodeIndices graph)
	# doms = dominators graph source
	# df = dominanceFrontiers` graph doms source
	= collectScopes [source] [] df newMap >>> \scopeMap -> 
	  translate source sink mergeNodes doms scopeMap
where
	collectScopes :: [NodeIndex] [NodeIndex] DFNodes ScopeMap -> GParseState ScopeMap
	collectScopes [] _ _ scopeMap= ret scopeMap
	collectScopes [node:rest] visited df scopeMap
		| isMember node visited = collectScopes rest visited df scopeMap
		= boundVariables node graph >>> \vars -> 
		  collectScopes (rest ++ directSuccessors node graph) [node:visited] df (updateScopeMap (fromMaybe [] (get node df)) vars scopeMap)

	boundVariables :: NodeIndex (Graph TreeGraphNode GEdge) -> GParseState [GIdentifier]
	boundVariables node graph
		# nodeData = fromJust (getNodeData node graph)
		# name = nodeName node graph
		| name == "let" = 
			getNodePattern (fromNode nodeData) 0 >>> \param -> ret (extractVariables param)
		# preds = directPredecessors node graph
		| length preds <> 1 = ret []
		# mPattern = (fromJust (getEdgeData (hd preds, node) graph)).GEdge.pattern
		| isNothing mPattern = ret []
		= ret (extractVariables (fromJust mPattern))

	domTreeChildren :: NodeIndex {#Int} -> [NodeIndex]
	domTreeChildren node doms = [ i \\ i <- [0.. size doms - 1] | doms.[i] == node && i <> node]

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

	translate :: NodeIndex NodeIndex [NodeIndex] {#Int} ScopeMap -> GParseState (AExpression Void)
	translate current sink mergeNodes doms scopeMap
		# c = domTreeChildren current doms
		# ps = filter (\p -> isMember p mergeNodes) [current:c]
		= parseMap (\i -> translate (hd (directSuccessors i graph)) sink mergeNodes doms scopeMap >>> \iexp -> 
		            ret ("f" +++ toString i +++ " " +++ unwords (getScope i scopeMap), iexp)) ps >>> \letExps ->
		  seqNodeToAExpr current sink mergeNodes doms scopeMap >>> \exp -> 
		  ret if (isEmpty letExps) exp (Let letExps exp)
			
	seqNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] {#Int} ScopeMap -> GParseState (AExpression Void)
	seqNodeToAExpr current sink mergeNodes doms scopeMap
		# nodeData = (fromJust (getNodeData current graph))
		| current == sink = treeGraphNodeToAExpression bindings nodeData                    //1
		| isNode nodeData
			# node = fromNode nodeData
			# nodename = node.GNode.name
			| isMember current mergeNodes = 
				let f = Var ("f" +++ toString current) in 
				ret (case fromMaybe [] (get current scopeMap) of
						[] = f
						xs = App [f : map Var xs]
					)
			| nodename == "case split" = caseNodeToAExpr current sink mergeNodes doms scopeMap //3
			| nodename == "let" = letNodeToAExpr current sink mergeNodes doms scopeMap      //4
			= seqNode` nodeData 
		= seqNode` nodeData
	where
		seqNode` nodeData
			# successors = directSuccessors current graph
			| not (isEmpty (tl successors)) = parseErrorInChild nodeData "Node cannot be used as split node"    //7
			# successor = hd successors
			| isPathToMergeSink successor sink = seqNodeToAExpr current current mergeNodes doms scopeMap
			# edgePattern = (fromJust (getEdgeData (current, successor) graph)).pattern                   //5,6
		    = treeGraphNodeToAExpression bindings nodeData      >>> \first`  -> 
		      seqNodeToAExpr successor sink mergeNodes doms scopeMap >>> \second` ->
			    ret (case edgePattern of 
		    			Just p  = (AppInfix ">>=" Infixl 1 first` (Lambda p second`))
				    	Nothing = (AppInfix ">>|" Infixl 1 first` second`)
			    	)

	caseNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] {#Int} ScopeMap -> GParseState (AExpression Void)
	caseNodeToAExpr current sink mergeNodes doms scopeMap
		# nodeData = fromJust (getNodeData current graph)
		# node = fromNode nodeData
		# successors = directSuccessors current graph
		| isEmpty successors = parseErrorInChild node "Missing node(s) after case split"
		| any (flip isPathToMergeSink sink) successors = parseErrorInChild node "Missing task in branch" //TODO: Highlight branch
		# caseExpression = node.GNode.actualParams
		= parseChild node (gToAExpression bindings (node.GNode.actualParams !! 0)) >>> \caseExpr -> 
		  parseMap (\n -> seqNodeToAExpr n sink mergeNodes doms scopeMap >>> \exp -> 
		            ret (fromJust (getEdgeData (current,n) graph), exp)) successors >>> \caseAlts ->
		  if (length (filter (\{pattern} -> isNothing pattern) (map fst caseAlts)) > 1)
			(parseErrorInChild node "A case expression can have at most one default case")
			//"otherwise" alternative is put at the end of the list
			//TODO: Sort according to diagram layout.
			(ret (let sortAlts = filter (not o isEmptyEdge o fst) caseAlts ++ filter (isEmptyEdge o fst) caseAlts
			    in Case caseExpr (map mkCaseAlt sortAlts)))
	where
		isEmptyEdge :: GEdge -> Bool
		isEmptyEdge {pattern = Nothing  } = True
		isEmptyEdge {pattern = Just pat } = size (trim pat) == 0
	
		mkCaseAlt :: (GEdge, AExpression Void) -> ACaseAlt Void
		mkCaseAlt ({pattern = Just pat}, exp) = CaseAlt pat exp
		mkCaseAlt ({pattern = Nothing }, exp)  = CaseAlt "otherwise" exp
		  
	letNodeToAExpr :: NodeIndex NodeIndex [NodeIndex] {#Int} ScopeMap -> GParseState (AExpression Void)
	letNodeToAExpr current sink mergeNodes doms scopeMap
		# nodeData = (fromJust (getNodeData current graph))
		# node = fromNode nodeData
		# successors = directSuccessors current graph
		| isEmpty successors = parseErrorInChild node "Missing node after let"
		| not (isEmpty (tl successors)) = parseErrorInChild node "Let cannot be used as split node"
		| isPathToMergeSink (hd successors) sink = parseErrorInChild node "Missing task in branch"
		= getNodePattern node 0 >>> \pat ->
		  gToAExpression bindings (node.GNode.actualParams !! 1) >>> \exp -> 
		  seqNodeToAExpr (hd successors) sink mergeNodes doms scopeMap >>> \inExpr ->
		  ret (Let [(pat,exp)] inExpr)

nodeName :: NodeIndex (Graph TreeGraphNode e) -> String
nodeName nodeIndex graph = case fromJust (getNodeData nodeIndex graph) of
	TGNode{ GNode | name } = name
	_                     = ""

getNodePattern :: GNode Int -> GParseState String
getNodePattern node index = parseChild node (getTextExpr (node.GNode.actualParams !! index))
where
	getTextExpr :: GExpression -> GParseState String
	getTextExpr (GCleanExpression exp) | exp == "" = parseError "Pattern is empty"
									   | otherwise = ret exp
	getTextExpr f = parseError "Expected: textual expression"
	
getNodes :: [NodeIndex] (Graph n e) -> [n]
getNodes nodes graph = catMaybes [ getNodeData n graph\\ n <- nodes ]

//--------------------------------------------------------------------------------------------------
/* 
 * Implementation of "A Simple, Fast Dominance Algorithm"
 * by Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy, 2001. 
 * Available at: http://www.cs.rice.edu/~keith/EMBED/dom.pdf
 */

/* 
 * mapIndicesReversePostorder: sort node indices of graph reverse postorder,
 * which is a prerequisite for dominance and dominanceFrontier functions
 */
mapIndicesReversePostorder :: (Graph n e) NodeIndex -> (Map NodeIndex NodeIndex, Graph n e)
mapIndicesReversePostorder graph startNode
	# indices = [ (i,j) \\ i <- reversePostorder graph startNode & j <- reverse [1 .. nodeCount graph] ]
	= (fromList indices, mapIndices indices graph)

reversePostorder :: (Graph n e) NodeIndex -> [NodeIndex]
reversePostorder graph startIndex = bfs graph [startIndex] []
where
	bfs :: (Graph n e) [NodeIndex] [NodeIndex] -> [NodeIndex]
	bfs graph [] visited = []
	bfs graph [n:ns] visited
		| isMember n visited = bfs graph ns visited
		= [n : bfs graph (ns ++ directSuccessors n graph) [n: visited]]

dominators :: (Graph n e) NodeIndex -> {#Int}
dominators graph startNode
	# allNodes = tl (reversePostorder graph startNode) // tl (reverse (sort (nodeIndices graph)))
	# doms = { createArray (maxList (nodeIndices graph) + 1) -1 & [startNode] = startNode }
	# (_, doms) = forAllNodes graph allNodes allNodes (False, doms)
	= doms

forAllNodes :: (Graph n e) ![Int] ![Int] (Bool, *{#Int}) -> (Bool, *{#Int})
forAllNodes graph [] allNodes (False,doms) = (False, doms)
forAllNodes graph [] allNodes (True,doms) = forAllNodes graph allNodes allNodes (False, doms)
forAllNodes graph [b:bs] allNodes (changed,doms)
	# ([new_idom:preds], doms) = filterProcessed (directPredecessors b graph) doms
	# (new_idom, doms) = forAllPredecessors preds (new_idom, doms)
	= if (doms.[b] <> new_idom)
		(forAllNodes graph bs allNodes (True   , { doms & [b] = new_idom }))
		(forAllNodes graph bs allNodes (changed, doms                     ))
where
	filterProcessed :: [Int] *{#Int} -> ([Int], *{#Int})
	filterProcessed [] doms = ([], doms)
	filterProcessed [n:ns] doms = 
		let (n`, doms`) = uselect doms n
	    in if (n` == -1)
			(filterProcessed ns doms`)
	        (let (ns`, doms``) = filterProcessed ns doms` in ([n:ns`], doms``))

forAllPredecessors :: ![Int] !(Int, *{#Int}) -> (Int, *{#Int})
forAllPredecessors [] (new_idom, doms) = (new_idom, doms)
forAllPredecessors [p:ps] (new_idom, doms)
	= 	let (p`, doms`) = uselect doms p
	    in if (p` == -1) 
	    		(forAllPredecessors ps (new_idom,doms`)) 
	    		(forAllPredecessors ps (intersect p new_idom doms`))

intersect :: !Int !Int !*{#Int} -> (Int, *{#Int})
intersect finger1 finger2 doms
	| finger1 == finger2 = (finger1, doms)
	# (finger1, doms) = reduce finger1 finger2 doms
	# (finger2, doms) = reduce finger2 finger1 doms
	= intersect finger1 finger2 doms
	
reduce :: !Int !Int !*{#Int} -> (Int, *{#Int})
reduce node stop doms
	| node >= stop = (node, doms)
	# (node`,doms`) = uselect doms node
	= reduce node` stop doms`

/*
dominates :: NodeIndex NodeIndex {#Int} -> Bool
dominates i j doms | i == j = True
                   | doms.[j] == i = True
                   | doms.[j] == j = False
                   = dominates i doms.[j] doms
*/

:: DFNodes :== Map NodeIndex [NodeIndex]

dominanceFrontiers :: (Graph n e) NodeIndex -> DFNodes
dominanceFrontiers graph startNode 
	# doms = dominators graph startNode
	= dominanceFrontiers` graph doms startNode

dominanceFrontiers` :: (Graph n e) {#Int} NodeIndex -> DFNodes
dominanceFrontiers` graph doms startNode
	= seq [ dfPred b doms \\ b <- filterNodes (\pred _ _ -> length pred >= 2) graph ] newMap
	where
		dfPred :: NodeIndex {#Int} DFNodes -> DFNodes
		dfPred b doms dfNodes = seq [run b doms p \\ p <- directPredecessors b graph] dfNodes

		run :: NodeIndex {#Int} NodeIndex DFNodes -> DFNodes
		run b doms runner dfNodes
			| runner <> doms.[b] = run b doms doms.[runner] 
				(put runner ((\dfs -> [b:removeMember b dfs]) (fromMaybe [] (get runner dfNodes))) dfNodes)
			| otherwise = dfNodes

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

//--------------------------------------------------------------------------------------------------
// Debugging

/*
traceDoms :: *{#Int} -> *{#Int}
traceDoms doms
	# (items, doms) = usize doms
	= trace "\nDoms:" seq [traceDoms` i \\ i <- [0..items - 1]] doms
where	
	traceDoms` :: Int *{#Int} -> *{#Int}
	traceDoms` index doms
		# (item, doms) = uselect doms index
		= trace (toString index +++ " -> " +++ toString item +++ " , ") doms

instance toString TreeGraphNode
where
	toString (TGNode {GNode | name}) = name
	toString (TGSubgraph s) = "(subgraph)"

instance toString (Maybe String)
where
	toString m = printToString m

derive gPrint Maybe

derive gPrint (,)

showTreeGraph :: TreeGraph -> String
showTreeGraph (TreeGraph graph source sink) = "[" +++ 
	toString (length (nodeIndices graph)) +++ " nodes, " +++ toString (length (edgeIndices graph)) +++ " edges, source=" +++ 
		toString source +++ ", sink=" +++ toString sink +++ ". " +++
	showTreeNodes [ (n,fromJust (getNodeData n graph)) \\ n <- nodeIndices graph ]
					+++ showEdges (edgeIndices graph) +++ "]"								
where
	showTreeNodes :: [(NodeIndex,TreeGraphNode)] -> String
	showTreeNodes [] = ""
	showTreeNodes [(i,TGNode n):ns] = toString i +++ "=" +++ n.GNode.name +++ "," +++ showTreeNodes ns
	showTreeNodes [(i,TGSubgraph tg):ns] = toString i +++ "=(" +++ showTreeGraph tg +++ ")" +++"," +++ showTreeNodes ns
	
	showEdges :: [EdgeIndex] -> String
	showEdges [] = ""
	showEdges [(fromNode,toNode):edges] = "(" +++ toString fromNode +++ "," +++ toString toNode +++ ")," +++ showEdges edges

instance toString GEdge
where
	toString edge = "GEdge"
*/

//--------------------------------------------------------------------------------------------------
//Utilities
    	
foldl1 :: (a a -> a) [a] -> a
foldl1 f [x:xs]  =  foldl f x xs

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x
unwords [x:xs] = x +++ " " +++ unwords xs
