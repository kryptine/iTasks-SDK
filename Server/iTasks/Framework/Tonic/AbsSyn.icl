implementation module iTasks.Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
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

mkGNode :: GNodeType -> GNode
mkGNode nt = {GNode|nodeType=nt}
