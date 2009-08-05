definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/
from TSt			import :: Workflow, :: Task
from Http			import :: HTTPRequest, :: HTTPResponse

from	iTasks import class iTask
import	GenPrint, GenParse, GUICore

/**
* Creates the iTasks system from a set of workflow definitions
*
* @param  A list of workflow definitions
* @return A list of predicate/handler pairs that can be plugged into a server
*         or CGI wrapper
*/
engine :: [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))]

/**
* Wraps any task and a label as a workflow with no access restrictions
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param The task
*/
workflow :: String (Task a) -> Workflow | iTask a