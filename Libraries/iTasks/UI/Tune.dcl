definition module iTasks.UI.Tune
/**
 * This module defines overloaded annotation operators
 * for tuning the UI of tasks and editors.
 */
from iTasks.UI.Layout import :: LayoutRule, :: LUI, :: LUINo, :: LUIMoves, :: LUIMoveID, :: LUIEffectStage
import iTasks.SDS.Definition

from iTasks.UI.Definition import :: UIAttribute, :: UIAttributes, :: Title, :: Hint, :: Label, :: Icon
from iTasks.WF.Definition import :: Task

/**
* Fine tune a task or editor by specifying custom layouts, tweaking generic layouts,
* or adding additional titles, hints and descriptions
*/
class tune option tunedItem :: !option !tunedItem -> tunedItem

/**
* Infix shorthands for the (overloaded) tune combinator.
*/
(<<@) infixl 2 :: !tunedItem !option    -> tunedItem | tune option tunedItem
(@>>) infixr 2 :: !option    !tunedItem -> tunedItem | tune option tunedItem

//* Overwriting attributes with constants
instance tune UIAttribute  (Task a)
instance tune UIAttributes (Task a)
instance tune UIAttribute  (Editor a w)
instance tune UIAttributes (Editor a w)

//* Common attributes
instance tune Title (Task a)
instance tune Hint  (Task a)
instance tune Label (Task a)
instance tune Icon  (Task a)
instance tune Title (Editor a w)
instance tune Hint  (Editor a w)
instance tune Label (Editor a w)
instance tune Icon  (Editor a w)

//* Apply a layout transform to a task
:: ApplyLayout = ApplyLayout !LayoutRule

instance tune ApplyLayout (Task a)
