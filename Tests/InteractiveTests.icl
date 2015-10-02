module InteractiveTests
/**
* This module contains a collection of interactive tests that need to
* be checked interactively by a human tester.
*/
import iTasks, TestFramework
import Tests.Interactive.CoreEditors
import Tests.Interactive.Editlets

suites = [testCoreEditors,testEditlets]

Start w = startEngine (runTests suites) w
