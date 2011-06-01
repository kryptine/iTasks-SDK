definition module GinCleanParser

from hashtable	import	::HashTable
from syntax		import	::ParsedDefinition

parseModule :: !String !Bool *File -> ([ParsedDefinition], *File)

