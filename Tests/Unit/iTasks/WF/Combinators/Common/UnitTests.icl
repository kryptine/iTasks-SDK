module iTasks.WF.Combinators.Common.UnitTests

import Data.Func, Data.Either
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe.GenPrint
import iTasks.Util.Testing

Start world = runUnitTests tests world

tests = [foreverStStateChange]

foreverStStateChange = testTaskOutput "foreverSt state change" task events exp checkEqual
where
	task   = foreverSt 0 (\st -> if (st < n) (return $ inc st) (viewInformation [] st))
	events = [Left ResetEvent]
	exp    = [TOUIChange $ ReplaceUI $ UI UITextView attrs []]
	where
		attrs =
			'Map'.fromList
				[("value", JSONString $ toString n), ("class", JSONArray [JSONString "step", JSONString "interact"])]
	n      = 100

derive gPrint TaskOutputMessage, UIChange, UIChildChange, UIAttributeChange, UI, UIType, Map, JSONNode
derive gEq TaskOutputMessage
