definition module NWorld
/**
* This module provides an NWorld. This is a state which encapsulates the world
* plus other IO handles
*/
from StdFile	import class FileSystem

from PrintUtil	import :: HtmlStream
from Gerda		import :: Gerda
from DataFile	import :: DataFile

:: *NWorld		= { worldC		:: *World			// world for any io
				  , inout		:: *HtmlStream		// to read from stdin and write to stdout
				  , gerda		:: *Gerda			// to read and write to a relational database
				  , datafile	:: *DataFile		// to read and write to a Clean database in a file
				  }

instance FileSystem NWorld

appWorldNWorld	:: !.(*World -> *World)       !*NWorld -> *NWorld
accWorldNWorld	:: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
				