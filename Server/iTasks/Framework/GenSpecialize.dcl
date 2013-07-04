definition module iTasks.Framework.GenSpecialize
/**
* This module provides utility functions to make it easier to specify specializations
* of the generic functions in the iTask class. This way you can easily create
* types with custom validations, and visualizations, without diving too deep into
* the inner workings of the generic machinery.
*
* The iTasks class contains the following generic functions:
*
* - JSONEncode & JSONDecode: For persistent storage
* - gVisualizeText: For visualizing values in textual forms
* - gVisualizeEditor & gUpdate: For creating user interfaces and processing edit events
* - gVerify: For checking properties of values
* - gDefault: For creating default values of a type
* - gEq: Defines equality.
* - gHeaders & gGridRows: For flatting a value to a tabular structure such as a grid.
*/

from Data.Maybe import :: Maybe
from StdGeneric import :: ConsPos
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode

from iTasks.Framework.GenVisualize import :: StaticVisualizationMode,:: VSt, :: VisualizationResult
from iTasks.Framework.GenVisualize import generic gVisualizeText, generic gVisualizeEditor, generic gHeaders, generic gGridRows
from iTasks.Framework.GenUpdate import generic gUpdate, generic gDefault

from iTasks.API.Core.SystemTypes import :: InteractionMask

//generic JSONEncode t :: !t -> [JSONNode]
//generic JSONDecode t :: ![JSONNode] -> (!Maybe t,![JSONNode])
customJSONEncode :: (a -> b) a -> [JSONNode] | JSONEncode{|*|} b
customJSONDecode :: (b -> a) [JSONNode] -> (Maybe a,![JSONNode]) | JSONDecode{|*|} b

//generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]
customGVisualizeText :: (a -> b) !StaticVisualizationMode !a -> [String] | gVisualizeText{|*|} b

//generic gVisualizeEditor a | gVisualizeText a, gHeaders a, gGridRows a, JSONEncode a, JSONDecode a :: !(Maybe a) !*VSt -> (!VisualizationResult,!*VSt)
customGVisualizeEditor :: (a -> b)  (Maybe a) !*VSt -> (!VisualizationResult,!*VSt) | gVisualizeEditor{|*|} b

//generic gUpdate a | gDefault a, JSONDecode a :: ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a,![InteractionMask])
customGUpdate :: (a -> b) (b -> a) ![Int] !JSONNode !(!a,!InteractionMask) -> (!a,!InteractionMask) | gUpdate{|*|} b

//generic gVerify a :: !(Maybe a) ![InteractionMask] !VerifyOptions -> ([VerifyMask],[InteractionMask])
//customGVerify :: ((b,[InteractionMask]) -> (a,[InteractionMask])) !(Maybe a) ![InteractionMask] !VerifyOptions -> ([VerifyMask],[InteractionMask]) | gVerify{|*|} b

//generic gEq a  :: a a -> Bool
//generic gDefault a :: [ConsPos] -> a

//generic gHeaders a :: a -> [String]
//generic gGridRows a | gVisualizeText a :: !a ![String] -> Maybe [String]

