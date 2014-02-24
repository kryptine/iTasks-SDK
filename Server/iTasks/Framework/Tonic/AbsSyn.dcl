definition module iTasks.Framework.Tonic.AbsSyn

from Data.Graph import :: Graph, :: Node
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GExpression, GListComprehension, TonicTask, ComprElem, CEType, TonicInfo,
  TonicIdent

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GExpression, GListComprehension, TonicTask, ComprElem, CEType, TonicInfo,
  TonicIdent

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GExpression, GListComprehension, TonicTask, ComprElem, CEType, TonicInfo,
  TonicIdent

:: TonicModule =
  { tm_name  :: String
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

:: TonicIdent
  = TEntryExitIds Int Int
  | TTaskId Int Int

:: TonicInfo =
  { tiModuleName   :: String
  , tiTaskName     :: String
  , tiValAsStr     :: Maybe String
  , tiIsBind       :: Bool
  , tiIdent        :: TonicIdent
  }

mkGNode :: GNodeType -> GNode

:: GIdentifier :== String

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
  |  GVar GCleanExpression

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

//:: TonicSyn
  //=  TaskApp Taskname [Arg]
  //|  Parallel ParallelType TonicSyn
  //|  Bind (Maybe Varname) TonicSyn TonicSyn
  //|  Assign Varname TonicSyn
  //|  TIf CleanCode TonicSyn TonicSyn
  //|  TCase CleanCode [(CleanCode, TonicSyn)]
  //|  TLet // TODO
  //|  TListCompr // TODO
  //|  TReturn TonicSyn
  //|  TStep // TODO
  //|  TList [TonicSyn]
  //|  TVar Varname
