implementation module GenRecord

import StdTuple, StdList, StdFunc, Error, Util, Shared, GenUpdate
from dynamic_string import copy_to_string, copy_from_string

copyRecord :: !a !b -> b | GenRecord a & GenRecord b
copyRecord src dst
	# srcFields = gGetRecordFields{|*|} src [] newMap
	= fst (gPutRecordFields{|*|} dst [] srcFields)
	
mapRecord :: !a -> b | GenRecord a & GenRecord, gUpdate{|*|} b
mapRecord rec
	# fields = gGetRecordFields{|*|} rec [] newMap
	= fst (gPutRecordFields{|*|} defaultValue [] fields)
	
generic gGetRecordFields r :: !r ![GenType] !*RecordFields -> *RecordFields

gGetRecordFields{|OBJECT of d|} fx (OBJECT o) _ fields
	| isRecordType d	= fx o (getFieldTypes d) fields
	| otherwise			= fields
gGetRecordFields{|CONS|} fx (CONS c) types fields = fx c types fields
gGetRecordFields{|EITHER|} fx fy either types fields = case either of
	LEFT x	= fx x types fields
	RIGHT y	= fy y types fields
gGetRecordFields{|PAIR|} fx fy (PAIR x y) types fields
	# fields = fx x types fields
	= fy y types fields
gGetRecordFields{|FIELD of d|} _ f types fields = put d.gfd_name (GenericDyn (copy_to_string f) (types !! d.gfd_index)) fields
gGetRecordFields{|UNIT|} _ _ fields = fields
gGetRecordFields{|Int|}		_ _ fields = fields
gGetRecordFields{|Real|}	_ _ fields = fields
gGetRecordFields{|Char|}	_ _ fields = fields
gGetRecordFields{|Bool|}	_ _ fields = fields
gGetRecordFields{|String|}	_ _ fields = fields
gGetRecordFields{|(->)|} _ _ _ _ fields = fields
gGetRecordFields{|Dynamic|} _ _ fields = fields
gGetRecordFields{|Shared|} _ _ _ _ fields = fields

derive gGetRecordFields [], Maybe, Either, (,), (,,), (,,,), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gGetRecordFields Note, Password, Date, Time, DateTime, Document, FormButton, Currency, User, UserDetails, Choice, MultipleChoice, Map, Tree, TreeNode
derive gGetRecordFields EmailAddress, Action, Table, HtmlDisplay, ButtonState

generic gPutRecordFields r :: !r ![GenType] !*RecordFields -> (!r,!*RecordFields)

gPutRecordFields{|OBJECT of d|} fx obj=:(OBJECT o) _ fields
	| isRecordType d	= appFst OBJECT (fx o (getFieldTypes d) fields)
	| otherwise			= (obj,fields)
gPutRecordFields{|CONS|} fx (CONS c) types fields = appFst CONS (fx c types fields)
gPutRecordFields{|EITHER|} fx fy either types fields = case either of
	LEFT x	= appFst LEFT (fx x types fields)
	RIGHT y	= appFst RIGHT (fy y types fields)
gPutRecordFields{|PAIR|} fx fy (PAIR x y) types fields
	# (x`,fields)	= fx x types fields
	# (y`,fields)	= fy y types fields
	= (PAIR x` y`,fields)
gPutRecordFields{|FIELD of d|} _ f types fields
	# (mbGenDyn,fields) = delU d.gfd_name fields
	# f` = case mbGenDyn of
		Just genDyn = case matchGenericDyn genDyn (types !! d.gfd_index) of
			Just f	= f
			Nothing	= f
		Nothing		= f
	= (f`,fields)
gPutRecordFields{|UNIT|} _ _ fields = (UNIT,fields)
gPutRecordFields{|Int|}		c _ fields = (c,fields)
gPutRecordFields{|Real|}	c _ fields = (c,fields)
gPutRecordFields{|Char|}	c _ fields = (c,fields)
gPutRecordFields{|Bool|}	c _ fields = (c,fields)
gPutRecordFields{|String|}	c _ fields = (c,fields)
gPutRecordFields{|(->)|} _ _ f _ fields = (f,fields)
gPutRecordFields{|Dynamic|} dyn _ fields = (dyn,fields)
gPutRecordFields{|Shared|} _ _ sha _ fields = (sha,fields)

derive gPutRecordFields [], Maybe, Either, (,), (,,), (,,,), Void, Display, Editable, Hidden, VisualizationHint, Timestamp
derive gPutRecordFields Note, Password, Date, Time, DateTime, Document, FormButton, Currency, User, UserDetails, Choice, MultipleChoice, Map, Tree, TreeNode
derive gPutRecordFields EmailAddress, Action, Table, HtmlDisplay, ButtonState

:: *RecordFields :== Map String GenericDyn
// This type is needed because dynamics can't be used inside generic functions.
// It includes the string representation of the value (generated by copy_to_string)
// and the generic type of it.
:: *GenericDyn = GenericDyn !*String !GenType

/**
* Tries to match & unpack a GenericDyn value.
*
* @param The GenericDyn
* @param The type to match
*
* @return The unpacked value if match succeeded.
*/
matchGenericDyn :: !*GenericDyn !GenType -> Maybe a
matchGenericDyn (GenericDyn str dynType) reqType
	| dynType === reqType	= Just (fst (copy_from_string str))
	| otherwise				= Nothing

// Retrieves the types of a record's fields.
getFieldTypes :: !GenericTypeDefDescriptor -> [GenType]
getFieldTypes {gtd_conses=c=:[{gcd_type}]} = getFieldTypes` gcd_type []
where
	getFieldTypes` (GenTypeArrow field next) acc	= getFieldTypes` next [field:acc]
	getFieldTypes` _ acc							= reverse acc
getFieldTypes _ = []
	
derive gEq GenType
derive bimap (,)