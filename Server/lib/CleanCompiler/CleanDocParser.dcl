definition module CleanDocParser

from Maybe		import	::Maybe
from Error		import	::MaybeErrorString, ::MaybeError
from hashtable	import	::HashTable
from syntax		import	::ParsedDefinition, ::ParsedExpr

parseModule :: !String !Bool *File -> ([ParsedDefinition], *File)

parseExpression :: !String *File -> (ParsedExpr, *File)
parseExpressionUnsafe :: !String -> Maybe ParsedExpr

:: ModuleComment = 
	{ description	:: !Maybe String
	}

emptyModuleComment :: ModuleComment
parseModuleComment :: !String -> MaybeErrorString ModuleComment

:: FunctionComment = 
	{ description	:: !Maybe String
	, params		:: ![ParamComment]
	, return		:: !Maybe String
	, throws	 	:: ![String]
	, gin			:: !Bool
	, title			:: !Maybe String
	, icon			:: !Maybe String
	, parallel		:: !Bool
	, shape			:: !Maybe String
	}

:: ParamComment = 
	{ name			:: !String
	, title			:: !Maybe String
	, description	:: !Maybe String																
	}
	
emptyFunctionComment :: FunctionComment
parseFunctionComment :: !String -> MaybeErrorString FunctionComment

:: TypeComment = 
	{ description	:: !Maybe String
	}

emptyTypeComment :: TypeComment
parseTypeComment :: !String -> MaybeErrorString TypeComment
