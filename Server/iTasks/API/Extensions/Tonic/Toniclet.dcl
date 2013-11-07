definition module iTasks.API.Extensions.Tonic.Toniclet

import iTasks
import iTasks.Framework.Tonic.AbsSyn
import iTasks.API.Core.Client.Editlet

derive gEditor
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gEditMeta
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gVisualizeText
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gDefault
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gUpdate
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

derive gVerify
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge, GExpression,
  GListComprehension, Graph, Node

toniclet :: GinGraph -> Editlet GinGraph GinGraph
