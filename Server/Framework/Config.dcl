definition module Config
/**
* This module provides a configuration file for the iTasks server.
* An initial default configuration is written when no config file is found.
*/
from Maybe import ::Maybe
from SystemTypes import :: Config

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