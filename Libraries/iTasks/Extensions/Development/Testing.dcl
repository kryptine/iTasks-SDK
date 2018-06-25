definition module iTasks.Extensions.Development.Testing
/**
* This module provides utilities for testing iTasks programs
*/
import iTasks

from Testing.TestEvents import :: EndEventType
from iTasks.Util.Testing import :: TestReport 

compileTestModule :: FilePath -> Task EndEventType
runTestModule :: FilePath -> Task EndEventType
