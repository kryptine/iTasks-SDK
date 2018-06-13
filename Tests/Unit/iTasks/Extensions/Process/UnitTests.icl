module iTasks.Extensions.Process.UnitTests

import iTasks.Extensions.Process
import iTasks, iTasks.Util.Testing
import iTasks.UI.Definition
import Testing.TestEvents

import System.OS, Data.Either, Data.Functor
import qualified Data.Set as DS
import qualified Data.Map as DM
import Text.GenJSON

derive JSONEncode TaskOutputMessage

Start world = runUnitTests
	[testCallFastProcess
	,testCallSlowProcess
 	] world

//Currently only tested on unix systems
testCallFastProcess = IF_WINDOWS
	(pass "Test call for fast process")
	(testTaskOutput "Test call fast process" sut events exp (\_ _ -> Passed)) //Only check if it does not crash
where
	sut = callProcess "Run fast process" [] "/bin/date" [] Nothing Nothing
	events = [Left ResetEvent,Right 1,Left (RefreshEvent 'DS'.newSet "Update")]
	exp = TOUIChange <$> [ReplaceUI initialUI,ReplaceUI finishedUI]

	initialUI = uiac UIContainer ('DM'.fromList [("stepped",JSONBool False)]) [toPrompt "Run fast process",uia UIProgressBar (textAttr "Running /bin/date...")]
	finishedUI = uic UIContainer [toPrompt "Run fast process",uia UIProgressBar (textAttr "/bin/date done (0)")]

testCallSlowProcess = IF_WINDOWS
	(pass "Test call for slow process")
	(testTaskOutput "Test call slow process" sut events exp (\_ _ -> Passed))
where
	sut = callProcess "Run slow process" [] "/bin/sleep" ["2"] Nothing Nothing
	events = [Left ResetEvent ,Right 1 ,Left (RefreshEvent 'DS'.newSet "Update"),Right 2,Left (RefreshEvent 'DS'.newSet "Update") ,Left (RefreshEvent 'DS'.newSet "Update")]
	exp = TOUIChange <$> [ReplaceUI initialUI, ReplaceUI finishedUI]

	initialUI = uic UIContainer [toPrompt "Run slow process",uia UIProgressBar (textAttr "Running /bin/sleep...")]
	finishedUI = uic UIContainer [toPrompt "Run slow process",uia UIProgressBar (textAttr "/bin/sleep done (0)")]


