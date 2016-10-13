module UnitTests
/**
* This module runs the unit tests on the command line
*/
import TestFramework
import System.CommandLine
import System.GetOpt

import Tests.Unit.CoreEditors
import Tests.Unit.Layout
import Tests.Unit.Editlets
import Tests.Unit.Misc
import Tests.Unit.TaskEvaluation

import Tests.Common.MinimalTasks

suites = [testGenericEditorGenUI
		 ,testGenericEditorEdits
		 ,testGenericEditorRefreshes
		 ,testGenericHelperFunctions
		 ,testLayout
		 ,testEditlets
		 ,testMisc
		 ,testTaskEvaluation
		 ]

//Commandline options 
:: CLIOpt = UseJSON | NameFilter String

Start world
	# (args,world) = getCommandLine world
	# (options,args,errors) = getOpt Permute [jsonOpt,nameFilterOpt] args
	# useJSON    = not (isEmpty [UseJSON \\ UseJSON <- options])
	# nameFilter = listToMaybe [f \\ NameFilter f <- options]
	# suites      = maybe suites (\f -> filterSuitesByTestName f suites) nameFilter
	| useJSON
		= runUnitTestsJSON suites world
	| otherwise
		= runUnitTestsCLI suites world
where
	jsonOpt = Option [] ["json"] (NoArg UseJSON) "Output testresults as JSON"
	nameFilterOpt = Option [] ["name"] (ReqArg NameFilter "*") "Only run tests that match a specific name"

