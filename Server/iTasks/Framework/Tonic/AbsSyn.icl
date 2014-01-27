implementation module iTasks.Framework.Tonic.AbsSyn

import Data.Graph
import Text.JSON
from GenEq import generic gEq

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive JSONDecode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

derive gEq
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, TonicTask, ComprElem, CEType, TonicInfo

mkGNode :: GNodeType -> GNode
mkGNode nt = {GNode|nodeType=nt, nodeTonicInfo = Nothing}
