definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from iTasks import class iTask, generic gEditor, generic gEditMeta,
  generic gUpdate, generic gVerify, generic gVisualizeText, generic gDefault,
  generic gEq, :: VSt, :: EditMeta, :: VisualizationResult, :: VerifiedValue,
  :: InteractionMask, :: DataPath, :: MaskedValue, :: Verification,
  :: VerifyOptions, :: VisualizationFormat

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

derive class iTask
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

//derive class iTask
  //TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  //GListComprehension, GExpression
