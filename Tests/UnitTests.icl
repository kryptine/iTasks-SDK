module UnitTests
/**
* This module runs the unit tests on the command line
*/
import TestFramework

import Tests.Unit.CoreEditors
import Tests.Unit.Layout
import Tests.Unit.Editlets
import Tests.Unit.Misc

suites = [testGenericEditorGenUI
		 ,testGenericEditorDiffs
		 ,testLayout
		 ,testEditlets
		 ,testMisc
		 ]

Start w = runUnitTestsCLI suites w
