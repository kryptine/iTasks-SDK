definition module EngineWrapperCGI
/**
* This module wraps the iTasks engine in a CGI protocol
* handler and creates an application that can be used together
* with an external webserver which supports CGI
*/
import Engine

/**
* Starts the task engine with a list of workflow definitions.
*
* @param A list of available workflows
* @param The world
* @return The world
*/
startEngine :: ![Workflow] !*World -> *World