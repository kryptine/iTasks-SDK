definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
import iTasks

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
  =  GInit
  |  GStop
  |  GDecision DecisionType !GCleanExpression
  |  GMerge
  |  GLet GLet
  |  GParallelSplit
  |  GParallelJoin GJoinType
  |  GTaskApp GIdentifier ![GExpression]
  |  GReturn !GExpression
  |  GAssign GCleanExpression
  |  GStep
  |  GListComprehension GListComprehension

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

derive class iTask
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

//derive class iTask
  //TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  //GListComprehension, GExpression
