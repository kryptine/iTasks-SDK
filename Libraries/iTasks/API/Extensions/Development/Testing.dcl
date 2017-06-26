definition module iTasks.API.Extensions.Development.Testing
/**
* This module provides utilities for testing iTasks programs
*/
import iTasks
from iTasks._Framework.Test.Definition import :: TestResult, :: SuiteResult

compileTestModule :: FilePath -> Task TestResult
runTestModule :: FilePath -> Task SuiteResult
