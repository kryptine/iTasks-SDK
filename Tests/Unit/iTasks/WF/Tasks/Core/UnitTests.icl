module iTasks.WF.Tasks.Core.UnitTests
import iTasks.Util.Testing
import qualified Data.Map as DM
import Data.Either
import Text.GenJSON

derive JSONEncode TaskOutputMessage

//Test interact
expPromptUI msg 
	= uiac UIContainer
		('DM'.fromList [("marginTop",JSONInt 5),("marginRight",JSONInt 5),("marginBottom",JSONInt 10),("marginLeft",JSONInt 5)
						,("width",JSONString "flex"),("minWidth",JSONString "wrap"),("height",JSONString "wrap")])
			[uia UITextView ('DM'.fromList [("value",JSONString msg)])]

minimalInteractUI = testTaskOutput "Initial UI of minimal interaction task" task events exp checkEqual
where
	task :: Task ((),String)
	task = interact "TEST" Update unitShare handlers gEditor{|*|}
	handlers = {onInit = \() -> ((),"Hello world"), onEdit = \_ l v -> (l,v,Nothing), onRefresh = \_ l v -> (l,v,Nothing)}

	events = [Left ResetEvent]
	exp = [TOUIChange (ReplaceUI expMinimalEditorUI)]

	expMinimalEditorUI
		= uic UIInteract [expPromptUI "TEST",editor]
	where
		editor = uia UITextField ('DM'.fromList
			[("hint-type",JSONString "valid")
			,("editorId",JSONString "v")
 			,("hint",JSONString "You have correctly entered a single line of text")
			,("optional",JSONBool False)
			,("mode",JSONString "update")
			,("taskId",JSONString "1-0")
			,("value",JSONString "Hello world")
			])

tests = [minimalInteractUI]

Start world = runUnitTests tests world
