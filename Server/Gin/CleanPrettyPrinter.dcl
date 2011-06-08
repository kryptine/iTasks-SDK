definition module CleanPrettyPrinter

from syntax import	::AType,
					::ParsedTypeDef,
					::RhsDefsOfType,
					::TypeContext,
					::TypeDef
					
from PPrint import	class Pretty,
					::Doc

printAType :: !Bool !AType  -> Doc

//instance Pretty TypeContext
instance Pretty [TypeContext]

instance Pretty ParsedTypeDef

instance Pretty Doc

prettyPrint :: a -> String | Pretty a


