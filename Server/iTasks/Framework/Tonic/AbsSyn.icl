implementation module iTasks.Framework.Tonic.AbsSyn

import Data.Map
import Text.JSON

JSONEncode{|GExpression|} GUndefinedExpression    = [JSONString "undef"]
JSONEncode{|GExpression|} (GGraphExpression g)    = [toJSON g]
JSONEncode{|GExpression|} (GListExpression ges)   = [toJSON ges]
JSONEncode{|GExpression|} (GCleanExpression gce)  = [JSONString gce]

derive JSONEncode
  TonicModule, GLet, DecisionType, GNode, GNodeType, GJoinType, GEdge,
  GListComprehension
