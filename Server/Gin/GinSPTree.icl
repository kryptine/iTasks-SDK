implementation module GinSPTree

import StdEnum
import StdList
import StdString
import GenPrint
        
import GinSyntax
import GinParser

graphToSPTree :: Bindings GGraph -> GParseState SPTree
graphToSPTree bindings graph 
# nodes = [0..(length graph.nodes - 1)]
= case filter (\n = isEmpty (getPredecessors graph n)) nodes  of
	[start] = case filter (\n = isEmpty (getSuccessors graph n)) nodes of
				  [end] = subgraphToTree bindings graph start end
				  []    = parseError "No end node found"
		          ends  = parseErrorInChildren "nodes" ends "End node is ambiguous"
	[]      = parseError "No start node found"  
	starts  = parseErrorInChildren "nodes" starts "Start node is ambiguous"

subgraphToTree :: Bindings GGraph Int Int -> GParseState SPTree
subgraphToTree bindings graph source sink
# aftersource = getSuccessors graph source
# beforesink = getPredecessors graph sink
= getDeclaration (getNode graph source).GNode.name bindings >>> \(branchtype, _) -> subgraphToTree` branchtype where
    aftersource :: [Int]
    aftersource = getSuccessors graph source
    beforesink :: [Int]
    beforesink = getPredecessors graph sink
    subgraphToTree` :: BranchType -> GParseState SPTree
    subgraphToTree` BTMerge = parseErrorInChildN "nodes" source "Merge unexpected"
    subgraphToTree` BTSingle | source == sink = getPathNode graph source >>> \source` = ret (SPNode source`)
    subgraphToTree` BTSingle | length aftersource == 1 = 
			subgraphToTree bindings graph (hd aftersource) sink >>> \tree = 
			getPathNode graph source >>> \source` = 
			getEdgePatternAfter graph source >>> \patternPath = 
			ret (SPSeries (SPNode source`) tree (getNodePatternAfter graph source) patternPath)
	subgraphToTree` BTSingle = parseErrorInChildN "nodes" source "Multiple outgoing connections are not allowed here"
	subgraphToTree` BTSplit | length aftersource == 0 = parseErrorInChildN "nodes" source "Missing outgoing connections"
	subgraphToTree` BTSplit =
			findsink bindings graph (hd aftersource) 0 >>> \parsink =
			matchPairs bindings graph aftersource (getPredecessors graph parsink) >>> \trees =
			getPathNode graph source >>> \source` = 
			getPathNode graph parsink >>> \parsink` = 
			if (parsink == sink) 
				(ret (SPParallel (source`,parsink`) trees))
				(let parsucc = getSuccessors graph parsink
				 in case length parsucc of 
				 	1 = subgraphToTree bindings graph (hd parsucc) sink >>> \tree = 
						getEdgePatternAfter graph parsink >>> \patternPath = 
						ret (SPSeries (SPParallel (source`,parsink`) trees) tree (getNodePatternAfter graph parsink) patternPath)
					otherwise = parseErrorInChildN "nodes" parsink "Multiple outgoing connections are not allowed here")


findsink :: Bindings GGraph Int Int -> GParseState Int
findsink bindings graph source level
= getDeclaration (getNode graph source).GNode.name bindings >>> \(branchtype, _) -> findsink` branchtype where
	aftersource :: [Int]
    aftersource = getSuccessors graph source
    findsink` :: BranchType -> GParseState Int
    findsink` BTMerge   | level == 0 = ret source
    findsink` _         | length aftersource == 0 = parseErrorInChildN "nodes" source "Could not find matching end node"
    findsink` BTSplit = findsink bindings graph (hd aftersource) (inc level)
    findsink` BTMerge = findsink bindings graph (hd aftersource) (dec level)
    findsink` BTSingle | length aftersource == 1 = findsink bindings graph (hd aftersource) level
    findsink` BTSingle = parseErrorInChildN "nodes" source "Multiple outgoing connections are not allowed here"

matchPairs :: Bindings GGraph [Int] [Int] -> GParseState [(SPPattern,SPTree)]
matchPairs _ _     [] []                  = ret []
matchPairs _ _     [] [sink:sinks]        = 
	parseErrorInChildN "nodes" sink "Node is not reachable from start"
matchPairs bindings graph [source:sources] sinks = 
	matchsource bindings graph source sinks >>> \(sink,tree) =
	matchPairs bindings graph sources (removeMember sink sinks) >>> \trees =
		ret [(getNodePatternBefore graph source,tree):trees]

//matchsource :: Graph Int [Int] -> GParseState (int, SPTree)
matchsource bindings graph source [sink:sinks] = 
	orElse (subgraphToTree bindings graph source sink >>> \tree = ret (sink, tree))
	       (matchsource bindings graph source sinks)
matchsource bindings _ source [] = parseErrorInChildN "nodes" source "Node cannot reach end node"

getPathNode :: GGraph Int -> GParseState SPPathNode
getPathNode graph index =  parseChildN "nodes" index getCurrentPath >>> \path = 
	ret (SPPathNode (getNode graph index) path)

getEdgePatternAfter :: GGraph Int -> GParseState GPath
getEdgePatternAfter graph index = parseChildN "edges" (hd (getSuccessorsEdges graph index)) (parseChild "pattern" getCurrentPath)

//instance toString SPTree where
//	toString tree = printToString tree
