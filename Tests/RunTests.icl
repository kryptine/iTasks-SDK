module RunTests
/**
* This module allows you to run all tests for the iTask system
* By default it starts an interactive, application for testing interactively by a human tester,
* but it can also be used to run just the unittests with output to the console.
*/
import System.CommandLine
import System.GetOpt

import iTasks
import TestFramework
import iTasks.UI.Definition
import Tests.Interactive.BuiltinEditors
import Tests.Interactive.GenericEditors
import Tests.Interactive.BuiltinContainers
import Tests.Interactive.CustomEditors
import Tests.Interactive.Layout
import Tests.Interactive.Editlets
import Tests.Interactive.CoreTasks
import Tests.Interactive.TaskPatterns

import Tests.Unit.CoreEditors
import Tests.Unit.Layout
import Tests.Unit.Editlets
import Tests.Unit.Misc
import Tests.Unit.TaskEvaluation
import Tests.Unit.CoreTasks

import Tests.Common.MinimalTasks

suites = [//Interactive tests
		  testBuiltinEditors
         ,testBuiltinEditorsWithShares
		 ,testGenericEditors
		 ,testBuiltinContainers
		 ,testCustomEditors
 		 ,testLayoutI
		 ,testEditletsI
         ,testCoreTasksI
         ,testTaskPatternsI
		 //Unit tests
		 ,testGenericEditorGenUI
		 ,testGenericEditorEdits
		 ,testGenericEditorRefreshes
		 ,testGenericHelperFunctions
         ,testCoreTasksUI
		 ,testLayout
		 ,testEditlets
		 ,testMisc
		 ,testTaskEvaluation
		 ,testCoreTasksUI 
		 ]

//Commandline options 
:: CLIOpt = UnitTestOnly | UseJSON | NameFilter String

Start world
	# (args,world) = getCommandLine world
	# (options,args,errors) = getOpt Permute [unitOpt,jsonOpt,nameFilterOpt] args
	# unitOnly     = not (isEmpty [UnitTestOnly \\ UnitTestOnly <- options])
	# useJSON      = not (isEmpty [UseJSON \\ UseJSON <- options])
	# nameFilter   = listToMaybe  [f \\ NameFilter f <- options]
	# suites       = maybe suites (\f -> filterSuitesByTestName f suites) nameFilter
	| unitOnly 
		= (if useJSON runUnitTestsJSON runUnitTestsCLI) suites world
	| otherwise
		= startEngine [publish "/" (\_ -> runTests suites <<@ ApplyLayout (setAttributes (titleAttr "iTasks Testbench")))
					  ,publish "/alternative" (const (viewInformation () [] "Alternative URL"))] world
where
	unitOpt = Option [] ["unit"] (NoArg UnitTestOnly) "Only run unit tests and show output on console"
	jsonOpt = Option [] ["json"] (NoArg UseJSON) "Output testresults as JSON"
	nameFilterOpt = Option [] ["name"] (ReqArg NameFilter "*") "Only run tests that match a specific name"
