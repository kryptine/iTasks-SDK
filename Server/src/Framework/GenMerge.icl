implementation module GenMerge

import StdGeneric, StdInt, StdReal, StdChar, StdBool, StdString, StdMisc, StdMaybe, Void, Either, Types

mergeValues :: a a a -> a | gMerge{|*|} a
mergeValues old cur new = getValue (gMerge{|*|} Merge old cur new)
	
:: MergeMode = Merge | Compare
:: MergeResult a = IsEqual Bool | Value a
derive gMerge [], Maybe, Either, (,), (,,), (,,,), Void, HtmlDisplay, Editable, Hidden
derive bimap MergeResult

generic gMerge a :: MergeMode a a a -> MergeResult a

gMerge{|Int|}		Merge old cur new	= mergeBasic old cur new
gMerge{|Int|}		Compare x y _		= IsEqual (x == y)
gMerge{|Real|}		Merge old cur new 	= mergeBasic old cur new
gMerge{|Real|}		Compare x y _  		= IsEqual (x == y)
gMerge{|Char|}		Merge old cur new 	= mergeBasic old cur new
gMerge{|Char|}		Compare x y _  		= IsEqual (x == y)
gMerge{|Bool|}		Merge old cur new	= mergeBasic old cur new
gMerge{|Bool|}		Compare x y _  		= IsEqual (x == y)
gMerge{|String|}	Merge old cur new 	= mergeBasic old cur new
gMerge{|String|}	Compare x y _  		= IsEqual (x == y)
gMerge{|Document|}	Merge old cur new	= mergeBasic old cur new
gMerge{|Document|} Compare x y _		= IsEqual (x == y)

gMerge{|OBJECT|}	f Merge (OBJECT old) (OBJECT cur) (OBJECT new)	= Value (OBJECT	(getValue (f Merge old cur new)))
gMerge{|OBJECT|}	f Compare (OBJECT x) (OBJECT y) _  				= IsEqual (isEqual (f Compare x y undef))
gMerge{|CONS|}		f Merge (CONS old) (CONS cur) (CONS new)		= Value (CONS	(getValue (f Merge old cur new)))
gMerge{|CONS|}		f Compare (CONS x) (CONS y) _ 					= IsEqual (isEqual (f Compare x y undef))
gMerge{|FIELD|}		f Merge (FIELD old) (FIELD cur) (FIELD new)		= Value (FIELD	(getValue (f Merge old cur new)))
gMerge{|FIELD|}		f Compare (FIELD x) (FIELD y) _  				= IsEqual (isEqual (f Compare x y undef))

gMerge{|PAIR|}		fx fy Merge (PAIR old0 old1) (PAIR cur0 cur1) (PAIR new0 new1)	= Value (PAIR  (getValue (fx Merge old0 cur0 new0)) (getValue (fy Merge old1 cur1 new1)))
gMerge{|PAIR|}		fx fy Compare (PAIR x1 y1) (PAIR x2 y2) _						= IsEqual (isEqual (fx Compare x1 x2 undef) && isEqual (fy Compare y1 y2 undef))
gMerge{|EITHER|}	fx fy Merge (LEFT old)  (LEFT cur)  (LEFT new)					= Value (LEFT  (getValue (fx Merge old cur new)))
gMerge{|EITHER|}	fx fy Merge (RIGHT old) (RIGHT cur) (RIGHT new)					= Value (RIGHT (getValue (fy Merge old cur new)))
gMerge{|EITHER|}	fx fy Merge (RIGHT old) (LEFT cur) _							= Value (LEFT cur)
gMerge{|EITHER|}	fx fy Merge (LEFT old) (RIGHT cur) _							= Value (RIGHT cur)
gMerge{|EITHER|}	fx fy Merge (LEFT old) (LEFT cur) new
	| isEqual (fx Compare old cur undef)											= Value new
	| otherwise																		= Value (LEFT cur)
gMerge{|EITHER|}	fx fy Merge (RIGHT old) (RIGHT cur) new
	| isEqual (fy Compare old cur undef)											= Value new
	| otherwise																		= Value (RIGHT cur)
gMerge{|EITHER|}	fl fr Compare (LEFT x) (LEFT y) _ 								= IsEqual (isEqual (fl Compare x y undef))
gMerge{|EITHER|}	fl fr Compare (RIGHT x) (RIGHT y) _ 							= IsEqual (isEqual (fr Compare x y undef))
gMerge{|EITHER|}	fl fr Compare _ _ _  											= IsEqual False

gMerge{|UNIT|} Merge UNIT UNIT UNIT	= Value UNIT
gMerge{|UNIT|} Compare UNIT UNIT _	= IsEqual True

mergeBasic :: a a a -> MergeResult a | == a
mergeBasic old cur new
	| old == cur	= Value new
	| otherwise		= Value cur
	
getValue :: (MergeResult a) -> a
getValue (Value a) = a

isEqual :: (MergeResult a) -> Bool
isEqual (IsEqual equal) = equal