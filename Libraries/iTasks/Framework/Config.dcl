definition module Config
/**
* This module provides a configuration file for the iTasks server.
* An initial default configuration is written when no config file is found.
*/

:: Config =
	{ clientPath	:: !String	// Where is the client located.
	, staticPath	:: !String	// Additional location where statically served content may be placed
	, rootPassword	:: !String	// Password for the 'root' superuser.
	, sessionTime	:: !Int		// Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, serverPort	:: !Int		// The TCP port the server runs on. Default is 80.
	, serverPath	:: !String	// The path at which the services are served (default /handlers)
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