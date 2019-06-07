definition module iTasks.UI.Tune
/**
 * This module defines overloaded annotation operators
 * for tuning the UI of tasks and editors.
 */
from iTasks.UI.Layout import :: LayoutRule, :: LUI, :: LUINo, :: LUIMoves, :: LUIMoveID, :: LUIEffectStage
import iTasks.SDS.Definition

from iTasks.UI.Definition import :: UIAttribute, :: UIAttributes
from iTasks.WF.Definition import :: Task

/**
* Fine tune a task or editor by specifying custom layouts, tweaking generic layouts,
* or adding additional titles, hints and descriptions
*/
class tune b f :: !b !(f a) -> f a
class tunev b a f | iTask a :: !(b a) !(f a) -> f a

/**
* Infix shorthands for the (overloaded) tune combinator.
*/
(<<@) infixl 2 :: !(f a) !b	-> f a | tune b f
(@>>) infixr 2 :: !b !(f a)	-> f a | tune b f
(<@@) infixl 2 :: !(f a) !(b a) -> f a | tunev b a f & iTask a
(@@>) infixr 2 :: !(b a) !(f a) -> f a | tunev b a f & iTask a

//* Overwriting attributes with constants
instance tune UIAttribute Task
instance tune UIAttributes Task
instance tune UIAttribute Editor
instance tune UIAttributes Editor

//* Apply a layout transform to a task
:: ApplyLayout = ApplyLayout LayoutRule
instance tune ApplyLayout Task

