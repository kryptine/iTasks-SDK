definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepCont, StepFilter, GParType,
  NodeContents

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepCont, StepFilter, GParType,
  NodeContents

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepCont, StepFilter, GParType,
  NodeContents

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_tasks :: Map TaskName TonicTask
  }

:: ModuleName :== String
:: VariableName :== String
:: TypeName :== String
:: TaskName :== String

:: TonicTask =
  { tt_name  :: TaskName
  , tt_resty :: TypeName
  , tt_args  :: [(VariableName, TypeName)]
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
  }

mkGNode :: GNodeType -> GNode

:: GIdentifier :== String

// TODO Add some sort of parallel arguments node type so we can simply replace
// the GNode in the actual graph node. Can be used when substituting variables
// at runtime. We don't need to create extra nodes for that in this graph; just
// in the rendering.
:: GNodeType
  =  GAssign GCleanExpression
  |  GDecision DecisionType GCleanExpression
  |  GInit
  |  GLet GLet
//  | GList [GCleanExpression]
  //|  GListComprehension GListComprehension
  |  GParallel GParType [NodeContents]
  |  GReturn NodeContents
  |  GStep [NodeContents]
  |  GStop
  |  GTaskApp GIdentifier ![GCleanExpression]
  |  GTransform GCleanExpression
  |  GVar GCleanExpression
  |  GArbitraryExpression

:: NodeContents
  = VarOrExpr GCleanExpression
  | ArbitraryOrUnknownExpr
  | Subgraph GinGraph
  | StepElem StepElem

:: GParType
  =  DisFirstBin
  |  DisFirstList
  |  DisLeft
  |  DisRight
  |  ConAll
  |  ConPair

:: StepElem
  = StepOnValue StepCont
  | StepOnButton String StepCont
  | StepOnException StepCont

:: StepCont =
  { stepContFilter :: StepFilter
  , stepContLbl    :: Maybe String
  , stepContNode   :: GNodeType
  }

:: StepFilter
  = StepAlways
  | StepNever
  | StepHasValue
  | StepIfStable
  | StepIfUnstable
  | StepIfValue
  | StepCond GCleanExpression

:: GEdge = { edge_pattern :: !Maybe GPattern }

:: GCleanExpression :== String

:: GListComprehension =
  {  output    :: GCleanExpression
  ,  guard     :: Maybe GCleanExpression
  ,  comprElem :: [ComprElem]
  //,  selector  :: GPattern
  //,  input     :: GCleanExpression
  }

:: ComprElem =
  { cePattern :: GPattern
  , ceType    :: CEType
  , ceInput   :: GCleanExpression
  }

:: CEType
  = ParComp
  | SeqComp
