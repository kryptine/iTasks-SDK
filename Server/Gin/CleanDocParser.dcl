definition module CleanDocParser

from Maybe		import	::Maybe
from Error		import	::MaybeErrorString, ::MaybeError
from hashtable	import	::HashTable
from syntax		import	::ParsedDefinition

parseModule :: !String !Bool *File -> ([ParsedDefinition], *File)

:: DocBlock = 
	{ description	:: !Maybe String
	, params		:: ![ParamDoc]
	, return		:: !Maybe String
	, throws	 	:: ![String]
	, gin			:: !Bool
	, title			:: !Maybe String
	, icon			:: !Maybe String
	, parallel		:: !Bool
	, shape			:: !Maybe String
	}

:: ParamDoc = 
	{ name			:: !String
	, title			:: !Maybe String
	, description	:: !Maybe String																
	}

emptyDocBlock :: DocBlock

parseDocBlock :: !String -> MaybeErrorString DocBlock
