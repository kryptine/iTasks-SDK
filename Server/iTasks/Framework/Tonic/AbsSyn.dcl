definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GExpression, GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GExpression, GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GExpression, GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

:: TonicModule =
  { tm_name  :: String
  //, tm_tasks :: Map String GinGraph
  , tm_tasks :: Map String TonicTask
  }

:: TonicTask =
  { tt_args  :: [String]
  , tt_graph :: GinGraph
  }

:: GinGraph :== Graph GNode GEdge

:: GPattern :== String

:: GLet =
  { glet_binds :: ![(GCleanExpression, GCleanExpression)]
  }

:: DecisionType = IfDecision | CaseDecision

:: GNode =
  { nodeType      :: !GNodeType
  , nodeTonicInfo :: Maybe TonicInfo
  }

:: TonicInfo =
  { tonicModuleName  :: String
  , tonicTaskName    :: String
  , tonicEntryUniqId :: Int
  , tonicExitUniqId  :: Int
  , tonicValAsStr    :: Maybe String
  }

mkGNode :: GNodeType -> GNode

:: GIdentifier :== String

// TODO Add some sort of parallel arguments node type so we can simply replace
// the GNode in the actual graph node. Can be used when substituting variables
// at runtime. We don't need to create extra nodes for that in this graph; just
// in the rendering.
:: GNodeType
  =  GAssign GCleanExpression
  |  GDecision DecisionType !GCleanExpression
  |  GInit
  |  GLet GLet
//  | GList [GExpression]
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
  //|  GListExpression [GExpression]
  //|  GListComprehensionExpression GListComprehension
  |  GCleanExpression GCleanExpression

:: GCleanExpression :== String

:: GListComprehension =
  {  output    :: GExpression
  ,  guard     :: Maybe GCleanExpression
  ,  comprElem :: [ComprElem]
  //,  selector  :: GPattern
  //,  input     :: GExpression
  }

:: ComprElem =
  { cePattern :: GPattern
  , ceType    :: CEType
  , ceInput   :: GExpression
  }

:: CEType
  = ParComp
  | SeqComp
