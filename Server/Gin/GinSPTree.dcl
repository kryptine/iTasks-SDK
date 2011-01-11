definition module GinSPTree

import GinSyntax
import GinParser

:: SPPathNode = SPPathNode GNode GPath

:: SPTree = SPNode SPPathNode
	| SPSeries SPTree SPTree SPPattern GPath
	| SPParallel (SPPathNode,SPPathNode) [(SPPattern,SPTree)]
	
:: SPPattern :== Maybe String

graphToSPTree :: Bindings GGraph -> GParseState SPTree

//instance toString SPTree
