definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension

:: TonicModule =
  { tm_name  :: String
  , tm_tasks :: Map String GinGraph
  }

:: GinGraph :== Graph GNode GEdge

:: GPattern :== String

:: GLet =
  { glet_binds :: ![(GCleanExpression, GCleanExpression)]
  }

:: DecisionType = IfDecision | CaseDecision

:: GNode =
  { nodeType :: !GNodeType
  }

:: GIdentifier :== String

:: GNodeType
  =  GAssign GCleanExpression
  |  GDecision DecisionType !GCleanExpression
  |  GInit
  |  GLet GLet
  |  GListComprehension GListComprehension
  |  GParallelSplit
  |  GParallelJoin GJoinType
  |  GReturn !GExpression
  |  GStep
  |  GStop
  |  GTaskApp GIdentifier ![GExpression]

:: GJoinType
  =  DisFirstBin
  |  DisFirstList
  |  DisLeft
  |  DisRight
  |  ConAll
  |  ConPair

:: GEdge = { edge_pattern :: !Maybe GPattern }

:: GExpression
  =  GUndefinedExpression
  |  GGraphExpression GinGraph
  |  GListExpression [GExpression]
  |  GListComprehensionExpression GListComprehension
  |  GCleanExpression GCleanExpression

:: GCleanExpression :== String

:: GListComprehension =
  {  output    :: GExpression
  ,  guard     :: Maybe GCleanExpression
  ,  selector  :: GPattern
  ,  input     :: GExpression
  }
