module InteractiveTests
/**
* This module contains a collection of interactive tests that need to
* be checked interactively by a human tester.
*/
import iTasks, TestFramework
import Tests.Interactive.CoreEditors
import Tests.Interactive.Layout
import Tests.Interactive.Editlets

import Tests.Common.MinimalTasks

suites = [testCoreEditors,testLayout,testEditlets]

Start w = startEngine [publish "/" (\_ -> runTests suites)
					  ,publishWithoutLayout "/minimal-suites"  (\_ -> runTests suites)
					  ,publishWithoutLayout "/minimal-editor"  (\_ -> minimalEditor)
					  ,publishWithoutLayout "/minimal-editlet" (\_ -> minimalEditlet)
					  ,publishWithoutLayout "/minimal-step"    (\_ -> minimalStep)
					  ] w
