module InteractiveTests
/**
* This module contains a collection of interactive tests that need to
* be checked interactively by a human tester.
*/
import iTasks, TestFramework
import Tests.Interactive.BuiltinEditors
import Tests.Interactive.GenericEditors
import Tests.Interactive.BuiltinContainers
import Tests.Interactive.CustomEditors
import Tests.Interactive.Layout
import Tests.Interactive.Editlets

import Tests.Unit.CoreEditors
import Tests.Unit.Layout
import Tests.Unit.Editlets
import Tests.Unit.Misc
import Tests.Unit.TaskEvaluation


import Tests.Common.MinimalTasks

suites = [//Interactive tests
		  testBuiltinEditors
		 ,testGenericEditors
		 ,testBuiltinContainers
		 ,testCustomEditors
 		 ,testLayoutI
		 ,testEditletsI
		 //Unit tests
		 ,testGenericEditorGenUI
		 ,testGenericEditorEdits
		 ,testGenericEditorRefreshes
		 ,testGenericHelperFunctions
		 ,testLayout
		 ,testEditlets
		 ,testMisc
		 ,testTaskEvaluation
		 ]

Start w = startEngine [publish "/" (\_ -> runTests suites)
					  ,publishWithoutLayout "/minimal-suites"  (\_ -> runTests suites)
					  ,publishWithoutLayout "/minimal-editor"  (\_ -> minimalEditor)
					  ,publishWithoutLayout "/minimal-editlet" (\_ -> minimalEditlet)
					  ,publishWithoutLayout "/minimal-step"    (\_ -> minimalStep)
					  ] w
