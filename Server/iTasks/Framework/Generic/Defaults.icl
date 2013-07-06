implementation module iTasks.Framework.Generic.Defaults

import StdGeneric, StdFunc
import Data.Maybe, Data.Either, Data.Void, Data.Map, Text.HTML, Text.JSON, System.Time

generic gDefault a :: [ConsPos] -> a

gDefault{|UNIT|} _							= UNIT
gDefault{|PAIR|} fa fb path					= PAIR (fa []) (fb [])
gDefault{|EITHER|} fa fb []					= LEFT (fa [])
gDefault{|EITHER|} fa fb [ConsLeft:path]	= LEFT (fa path)
gDefault{|EITHER|} fa fb [ConsRight:path]	= RIGHT (fb path)
gDefault{|OBJECT|} fa _						= OBJECT (fa [])
gDefault{|CONS|} fa	_						= CONS (fa [])
gDefault{|RECORD|} fa _						= RECORD (fa [])
gDefault{|FIELD|} fa _						= FIELD (fa [])

gDefault{|Int|}	_							= 0
gDefault{|Real|} _							= 0.0
gDefault{|Char|} _							= '\0'
gDefault{|Bool|} _							= False
gDefault{|String|} _						= ""
gDefault{|[]|} _ _							= []
gDefault{|(,)|} fa fb _						= (fa [],fb [])
gDefault{|(,,)|} fa fb fc _					= (fa [],fb [],fc [])
gDefault{|(,,,)|} fa fb fc fd _				= (fa [],fb [],fc [],fd [])
gDefault{|(->)|} fa fb _					= const (fb [])
gDefault{|Dynamic|}	_						= dynamic 42
gDefault{|Maybe|} fa _						= Nothing

gDefault{|HtmlTag|} _						= Html ""

derive gDefault Either, Void, Map, JSONNode, Timestamp

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|} []