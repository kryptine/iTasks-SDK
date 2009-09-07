definition module Config
/**
* This module provides a configuration file for the iTasks server.
* An initial default configuration is written when no config file is found.
*/

:: Config =
	{ clientPath	:: !String	// Where is the client located.
	, rootPassword	:: !String	// Password for the 'root' superuser.
	, sessionTime	:: !Int		// Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, debug			:: !Bool	// Run the server in debug mode.
	}

/**
* Load the config from disk, or create one based on the default config.
* 
* @param The application name
* @param The world
*
* @return The configuration data
* @return The world
*/
loadConfig :: !String !*World -> (!Config, !*World)