definition module iTasks.WF.Combinators.Tune

/**
 * This module provides combinators for tuning tasks and editors.
 *
 * Setting attributes on tasks:
 * Attributes are accessible in parallel task-lists as subtask meta-data
 * and they are added to the task UI.
 *
 * Setting attributes on editors: Only adds them to the UI of the editor.
 */

import iTasks.WF.Definition
from iTasks.UI.Layout import :: LayoutRule, :: LUI, :: LUINo, :: LUIMoves, :: LUIMoveID, :: LUIEffectStage
from Text.GenJSON import :: JSONNode
import iTasks.SDS.Definition


class addConstantAttribute f :: !String !b !(f a) -> f a | toAttribute b
instance addConstantAttribute Task
//instance addConstantAttribute Editor

//Setting attributes based on a task value or
class addValueAttribute f :: !String ((Maybe a) -> b) !(f a) -> f a | toAttribute b
instance addValueAttribute Task
//instance addConstantAttribute Editor

//Setting attributes based on an SDS value (only possible for tasks)
class addSDSAttribute f :: !String !sds !(r -> b) !(f a) -> f a | toAttribute b & TC r & Registrable sds () r
instance addSDSAttribute Task

class toAttribute a where toAttribute :: a -> String
instance toAttribute String
instance toAttribute Int

/**
* Fine tune a task by specifying custom layouts, tweaking generic layouts,
* or add additional titles, hints and descriptions
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

:: ApplyAttribute a = ApplyAttribute String a
instance tune (ApplyAttribute a) Task | toAttribute a

:: ApplySDSAttribute a r = E. sds: ApplySDSAttribute !String !sds !(r -> a) & Readable sds () r & Registrable sds () r
instance tune (ApplySDSAttribute a r) Task | toAttribute a & TC r

//* Apply a layout to a task
applyLayout :: LayoutRule (Task a) -> Task a

:: ApplyLayout	= ApplyLayout LayoutRule
instance tune	ApplyLayout Task

