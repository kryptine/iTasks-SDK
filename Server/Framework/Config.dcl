definition module Config
/**
* This module provides a configuration file for the iTasks server.
* An initial default configuration is written when no config file is found.
*/
import StdMaybe

:: Config =
	{ clientPath		:: !String			// Where is the client located.
	, staticPath		:: !String			// Additional location where statically served content may be placed
	, rootPassword		:: !String			// Password for the 'root' superuser (default 'root').
	, rootEmail			:: !String			// E-mail address for the 'root' superuser (default root@localhost).
	, sessionTime		:: !Int				// Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, serverPort		:: !Int				// The TCP port the server runs on. Default is 80.
	, serverPath		:: !String			// The path at which the services are served (default /services)
	, debug				:: !Bool			// Run the server in debug mode (default False).
	, smtpServer		:: !String			// The smtp server to use for sending e-mails
	, generalWorkflows	:: !Bool			// Enable the "general" workflows for managing ad-hoc work
	}

/**
* Returns the default configuration
*
* @return Default configuration
*/
defaultConfig :: Config
/**
* Load the config from disk.
* 
* @param The application name
* @param The world
*
* @return The configuration data
* @return The world
*/
loadConfig :: !String !*World -> (!Maybe Config, !*World)
/**
* Writes the config to disk
*
* @param The application name
* @param The configuration data
* @param The world
*
* @return The world
*/
storeConfig :: !String !Config !*World -> *World