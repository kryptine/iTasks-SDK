module CoreTasks
import iTasks, iTasks._Framework.Test.Definition
import iTasks.UI.Definition
import iTasks.Extensions.Process
import System.OS, Data.Either

derive gPrettyTrace UIChange, UIChildChange, UIAttributeChange, UI, UINodeType, JSONNode

Start world = execTestSuite (testsuite "UIs of core tasks" "Tests for UI behavior of core tasks" 
	[skip (testCallFastProcess)
	,skip (testCallSlowProcess)
 	]) world

//Currently only tested on unix systems
testCallFastProcess = IF_WINDOWS (pass "Test call for fast process") (testTaskOutput "Test call fast process" tut events exp checkEqual)
where
	tut = callProcess "Run fast process" [] "/bin/date" [] Nothing
	events = [Left ResetEvent,Right 1,Left (RefreshEvent "Update")]
	exp = [ReplaceUI initialUI,ReplaceUI finishedUI]

	initialUI = uic UIContainer [toPrompt "Run fast process",uia UIProgressBar (textAttr "Running /bin/date...")]
	finishedUI = uic UIContainer [toPrompt "Run fast process",uia UIProgressBar (textAttr "/bin/date done (0)")]

testCallSlowProcess = IF_WINDOWS (pass "Test call for slow process") (testTaskOutput "Test call slow process" tut events exp checkEqual)
where
	tut = callProcess "Run slow process" [] "/bin/sleep" ["2"] Nothing
	events = [Left ResetEvent,Right 1,Left (RefreshEvent "Update"),Right 2,Left (RefreshEvent "Update"),Left (RefreshEvent "Update")]
	exp = [ReplaceUI initialUI, ReplaceUI finishedUI]

	initialUI = uic UIContainer [toPrompt "Run slow process",uia UIProgressBar (textAttr "Running /bin/sleep...")]
	finishedUI = uic UIContainer [toPrompt "Run slow process",uia UIProgressBar (textAttr "/bin/sleep done (0)")]


