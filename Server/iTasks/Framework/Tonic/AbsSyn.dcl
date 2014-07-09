definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepFilter, GParType,
  NodeContents, TTaskApp, StepCond

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepFilter, GParType,
  NodeContents, TTaskApp, StepCond

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GEdge, GListComprehension,
  TonicTask, ComprElem, CEType, StepElem, StepFilter, GParType,
  NodeContents, TTaskApp, StepCond

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
  =  GAssign GCleanExpression NodeContents
  |  GDecision DecisionType GCleanExpression
  |  GInit
  |  GLet GLet
//  | GList [GCleanExpression]
  //|  GListComprehension GListComprehension
  |  GParallel GParType [NodeContents]
  |  GReturn NodeContents
  |  GStop
  |  GParSum
  |  GParProd
  |  GStepStar
  |  GStepElem StepElem
  |  GStepCond StepCond
  |  GTaskApp TTaskApp
  |  GTransform GCleanExpression
  |  GVar GCleanExpression
  |  GArbitraryExpression

:: TTaskApp =
  { taskApp_taskName :: GIdentifier
  , taskApp_args     :: [GCleanExpression]
  }

:: NodeContents
  = VarOrExpr GCleanExpression
  | Subgraph GinGraph
  | StepElem StepElem
  | NodeTaskApp TTaskApp

:: GParType
  =  DisFirstBin
  |  DisFirstList
  |  DisLeft
  |  DisRight
  |  ConAll
  |  ConPair

:: StepElem
  = StepOnValue StepFilter
  | StepOnAction ButtonText StepFilter
  | StepOnException

:: ButtonText :== String

:: EdgeLabel :== String

:: StepFilter
  = StepAlways
  | StepNever
  | StepHasValue
  | StepIfStable
  | StepIfUnstable
  | StepIfValue
  | StepIfCond

:: StepCond =
  { tifv_funName :: String
  , tifv_args    :: [String]
  }

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
