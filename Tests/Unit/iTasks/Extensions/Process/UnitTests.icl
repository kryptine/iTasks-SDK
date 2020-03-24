module iTasks.Extensions.Process.UnitTests

import iTasks.Extensions.Process
import iTasks, iTasks.Util.Testing
import iTasks.UI.Definition
import Testing.TestEvents

import System.OS, Data.Either, Data.Functor
import qualified Data.Set as DS
import qualified Data.Map as DM

import Text.GenPrint

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
	sut = callProcess [] "/bin/date" [] Nothing Nothing
	events = [Left ResetEvent,Right 1,Left (RefreshEvent 'DS'.newSet)]
	exp = TOUIChange <$> [ReplaceUI initialUI,ReplaceUI finishedUI]

	initialUI = uia UIProgressBar (textAttr "Running /bin/date...")
	finishedUI = uia UIProgressBar (textAttr "/bin/date done (0)")

testCallSlowProcess = IF_WINDOWS
	(pass "Test call for slow process")
	(testTaskOutput "Test call slow process" sut events exp (\_ _ -> Passed))
where
	sut = callProcess [] "/bin/sleep" ["2"] Nothing Nothing
	events = [Left ResetEvent ,Right 1 ,Left (RefreshEvent 'DS'.newSet),Right 2,Left (RefreshEvent 'DS'.newSet) ,Left (RefreshEvent 'DS'.newSet)]
	exp = TOUIChange <$> [ReplaceUI initialUI, ReplaceUI finishedUI]

	initialUI = uia UIProgressBar (textAttr "Running /bin/sleep...")
	finishedUI = uia UIProgressBar (textAttr "/bin/sleep done (0)")


