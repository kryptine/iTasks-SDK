definition module NWorld
/**
* This module provides an NWorld. This is a state which encapsulates the world
* plus other IO handles
*/
from StdFile	import class FileSystem

from DataFile	import :: DataFile

:: *NWorld		= { world		:: *World			// world for any io
				  , datafile	:: *DataFile		// to read and write to a Clean database in a file
				  }

instance FileSystem NWorld

mkNWorld		:: *World *DataFile -> *NWorld

appWorldNWorld	:: !.(*World -> *World)			!*NWorld -> *NWorld
accWorldNWorld	:: !.(*World -> *(.a,*World))	!*NWorld -> (.a,!*NWorld)