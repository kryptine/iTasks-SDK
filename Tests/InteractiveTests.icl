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

Start w = startEngine [publish "/" (WebApp []) (\_ -> withFinalSessionLayout (runTests suites))
					  ,publish "/minimal-editor" (WebApp []) (\_ -> minimalEditor <<@ WithoutAutoLayout)
					  ,publish "/minimal-editlet" (WebApp []) (\_ -> minimalEditlet <<@ WithoutAutoLayout)
					  ,publish "/minimal-step" (WebApp []) (\_ -> minimalStep <<@ WithoutAutoLayout)
					  ] w
