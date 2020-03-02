module iTasks.WF.Tasks.Core.UnitTests
import iTasks.Util.Testing
import qualified Data.Map as DM
import Data.Either, Data.Maybe
import Data.Maybe.GenPrint
import Text.GenPrint

derive gPrint TaskOutputMessage
derive gPrint UIChange, UIChildChange, UIAttributeChange, UI, UIType, Map, JSONNode
derive gEq TaskOutputMessage

minimalInteractUI = skip (testTaskOutput "Initial UI of minimal interaction task" task events exp checkEqual)
where
	task :: Task String
	task = withShared (Just "Hello world") \sds -> interactRW gEditor{|*|} sds 

	events = [Left ResetEvent]
	exp = [TOUIChange (ReplaceUI expMinimalEditorUI)]

	expMinimalEditorUI = editor
	where
		editor = uia UITextField ('DM'.fromList
			[("hint-type",JSONString "valid")
			,("editorId",JSONString "v")
 			,("hint",JSONString "You have correctly entered a single line of text")
			,("optional",JSONBool False)
			,("mode",JSONString "update")
			,("taskId",JSONString "1-0")
			,("value",JSONString "Hello world")
			,("minlength",JSONInt 1)
			,("task-type",JSONString "interact")
			])

tests = [minimalInteractUI]

Start world = runUnitTests tests world
