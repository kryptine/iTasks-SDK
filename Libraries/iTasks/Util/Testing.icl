implementation module iTasks.Util.Testing

import iTasks, StdFile, StdMisc
import iTasks.Extensions.Image
import iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Common, iTasks.UI.Definition
import iTasks.Extensions.Editors.Ace
import iTasks.Internal.Serialization
import Text, Text.HTML, System.CommandLine
import qualified Data.Map as DM
import iTasks.Extensions.Development.Codebase
import Data.Func, Data.Either, Data.Error

from iTasks.Internal.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{options} 
from iTasks.Internal.TaskStore import createTaskInstance, taskInstanceOutput, :: TaskOutput, :: TaskOutputMessage
from iTasks.Internal.TaskEval import evalTaskInstance
from iTasks.Internal.Store import emptyStore
from iTasks.Internal.Util import toCanonicalPath
import iTasks.Internal.Serialization
import iTasks.Internal.IWorld
import iTasks.UI.Definition
import qualified iTasks.Internal.SDS as SDS
from Data.Queue import :: Queue(..)
import System.OS
import iTasks.Util.Trace

import Testing.TestEvents

derive class iTask InteractiveTest

gText{|UnitTest|} _ _			            = []
gEditor{|UnitTest|} = emptyEditor 
JSONEncode{|UnitTest|} _ c	   = [dynamicJSONEncode c]
JSONDecode{|UnitTest|} _ [c:r] = (dynamicJSONDecode c,r)
JSONDecode{|UnitTest|} _ r	   = (Nothing,r)
gEq{|UnitTest|} _ _			   = True
gDefault{|UnitTest|}		   = {UnitTest|name="Default unit test",test=pass}
where
	pass :: *World -> *(EndEventType,*World)
	pass w = (Passed,w)

assert :: String (a -> Bool) a -> UnitTest | JSONEncode{|*|} a
assert name exp sut = {UnitTest|name=name,test=test}
where
	test w = (if (exp sut) Passed (Failed Nothing),w)


assertEqual :: String a a -> UnitTest | gEq{|*|} a & JSONEncode{|*|} a
assertEqual name exp sut = {UnitTest|name=name,test=test}
where
	test w = (checkEqual exp sut,w)

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> UnitTest | JSONEncode{|*|} a
assertWorld name exp sut = {UnitTest|name=name,test=test}
where
	test w 
		# (res,w) = sut w
		= (if (exp res) Passed (Failed Nothing),w)

assertEqualWorld :: String a (*World -> *(a,*World)) -> UnitTest | gEq{|*|} a & JSONEncode{|*|} a
assertEqualWorld name exp sut = {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp === res) Passed (Failed (Just (FailedAssertions [ExpectedRelation (toJSON exp) Eq (toJSON res)]))),w)

checkEqual :: a a -> EndEventType | gEq{|*|} a & JSONEncode{|*|} a
checkEqual exp sut = checkEqualWith (===) exp sut

checkEqualWith :: (a a -> Bool) a a -> EndEventType | JSONEncode{|*|} a
checkEqualWith pred exp sut = if (pred exp sut) Passed (Failed (Just (FailedAssertions [ExpectedRelation (toJSON exp) Eq (toJSON sut)])))

pass :: String -> UnitTest
pass name = {UnitTest|name=name,test = \w -> (Passed,w)}

fail :: String -> UnitTest
fail name = {UnitTest|name=name,test = \w -> (Failed Nothing, w)}

skip :: UnitTest -> UnitTest
skip skipped=:{UnitTest|name} = {UnitTest|name=name,test= \w -> (Skipped,w)}

filterTestsByName :: String [UnitTest] -> [UnitTest]
filterTestsByName pattern tests = filter (\{UnitTest|name} -> indexOf pattern name >= 0) tests

//UTILITY TASKS
testEditor :: (Editor a) a EditMode -> Task a | iTask a
testEditor editor model mode
	=   (interact "Editor test" mode unitShare {onInit = const ((),model), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \_ l v -> (l,v,Nothing)} editor @ snd
	>&> viewSharedInformation "Editor value" [ViewAs (toString o toJSON)] @? tvFromMaybe
	)  <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal) )

testEditorWithShare :: (Editor a) a EditMode -> Task a | iTask a
testEditorWithShare editor model mode = (withShared model
	\smodel ->
		updateSharedInformation "Edit the shared source" [] smodel 
		||-
	    interact "Editor under test" mode smodel {onInit = \r -> ((),r)
												 ,onEdit = \v l _ -> (l,v,Just (\_ -> v))
												 ,onRefresh = \r l v -> (l,r,Nothing)} editor @ snd
	) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal)) 

testCommonInteractions :: String -> Task a | iTask a
testCommonInteractions typeName
	= 	 enterInformation ("Enter","Enter information of type " +++ typeName) []
	-||- updateInformation ("Update","Update default value of type " +++ typeName) [] defaultValue
	-||- (withShared defaultValue
			\s -> (updateSharedInformation ("Update shared","Update shared value of type " +++ typeName) [] s
				   -||
				   viewSharedInformation ("View shared","View shared value of type " +++ typeName) [] s
				  )
		 )

testTaskOutput :: String (Task a) [Either Event Int] [TaskOutputMessage] ([TaskOutputMessage] [TaskOutputMessage] -> EndEventType) -> UnitTest | iTask a
testTaskOutput name task events exp comparison = {UnitTest|name=name,test=test}
where
	test world 
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld options world
		//Initialize JS compiler support
		# (res,iworld) = initJSCompilerState iworld
		| res =:(Error _)
			= (Failed (Just Crashed),destroyIWorld iworld)
		//Empty the store to make sure that we get a reliable task instance no 1
		# iworld = emptyStore iworld
		//Create an instance with autolayouting disabled at the top level
		# (res,iworld) = createTaskInstance task iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				//Apply all events
				# (res,iworld) = applyEvents instanceNo events iworld 
				= case res of
					(Ok ())
						//Collect output
						# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceOutput) iworld
						# world = destroyIWorld iworld
						//Compare result
						# verdict = case res of
							Ok queue = comparison exp (toList queue)
							(Error (_,e)) = Failed (Just Crashed)
						= (verdict,world)
					(Error e)
						# world = destroyIWorld iworld
						= (Failed (Just Crashed),world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just Crashed),world)

	applyEvents _ [] iworld = (Ok (),iworld)
	applyEvents instanceNo [Left e:es] iworld
		= case evalTaskInstance instanceNo e iworld of
			(Ok _,iworld) = applyEvents instanceNo es iworld
			(Error e,iworld) = (Error e,iworld)
	applyEvents instanceNo [Right e:es] iworld
		//Wait between events
		# iworld = (sleep e) iworld
		= applyEvents instanceNo es iworld

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear

	//TODO: Do this with a platform independent standard function
	sleep secs iworld = IF_POSIX (sleep_posix secs iworld) iworld
	sleep_posix secs iworld
		# x = sleep` secs
		| x == 0 && x <> 0 = undef
		= iworld
	where
       sleep` :: !Int -> Int
       sleep` secs = code {
          ccall sleep "I:I"
       }

allPassed :: TestReport -> Bool
allPassed report = checkSuiteResult (\r -> r =: Passed) report

noneFailed :: TestReport -> Bool
noneFailed report = checkSuiteResult (\r -> r =: Passed || r =: Skipped) report

checkSuiteResult :: (EndEventType -> Bool) [(String,EndEventType)] -> Bool
checkSuiteResult f testResults = all (\(_,r) -> f r) testResults

runUnitTests :: [UnitTest] *World -> *World
runUnitTests suites world
	# (console,world)	       = stdio world
	# (report,(console,world)) = foldl runTest ([],(console,world)) suites
	# (_,world)			       = fclose console world
	# world 			       = setReturnCode (if (noneFailed report) 0 1) world
    = world
where	
	runTest (results,(console,world)) {UnitTest|name,test}
		# console = fwrites (toString (toJSON (StartEvent {StartEvent|name=name})) +++ "\n") console
		//# console = fwrites (name +++ "... ") console
		# (result,world) = test world
		# message = case result of
			Passed = green "PASSED" 
			Failed _ = red "FAILED"
			Skipped = yellow "SKIPPED" 
		# console = fwrites (toString (toJSON (EndEvent {EndEvent|name=name,event=result,message=message})) +++ "\n") console
		= ([(name,result):results],(console,world))

	//ANSI COLOR CODES -> TODO: Create a library in clean-platform for ANSI colored output
	red s = toString [toChar 27,'[','3','1','m'] +++ s +++ toString [toChar 27,'[','0','m']
	green s = toString [toChar 27,'[','3','2','m'] +++ s +++ toString [toChar 27,'[','0','m']
	yellow s = toString [toChar 27,'[','3','3','m'] +++ s +++ toString [toChar 27,'[','0','m']

