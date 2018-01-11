implementation module iTasks.Internal.Generic.Defaults

import StdGeneric, StdFunc
import Data.Maybe, Data.Either, Data.Error, Data.Map, Text.HTML, Text.JSON, System.Time

generic gDefault a :: a

gDefault{|UNIT|}							    = UNIT
gDefault{|PAIR|} fa fb                          = PAIR fa fb
gDefault{|EITHER|} fa fb					    = LEFT fa       //Choose first constructor
gDefault{|OBJECT|} fa					        = OBJECT fa
gDefault{|CONS|} fa						        = CONS fa
gDefault{|RECORD|} fa						    = RECORD fa
gDefault{|FIELD|} fa						    = FIELD fa

gDefault{|Int|}							        = 0
gDefault{|Real|}							    = 0.0
gDefault{|Char|}							    = '-'
gDefault{|Bool|}						        = False
gDefault{|String|}						        = ""
gDefault{|[]|} fa							    = []
gDefault{|()|} 						            = ()
gDefault{|(,)|} fa fb						    = (fa,fb)
gDefault{|(,,)|} fa fb fc					    = (fa,fb,fc)
gDefault{|(,,,)|} fa fb fc fd				    = (fa,fb,fc,fd)
gDefault{|(,,,,)|} fa fb fc fd fe			    = (fa,fb,fc,fd,fe)
gDefault{|(,,,,,)|} fa fb fc fd fe ff		    = (fa,fb,fc,fd,fe,ff)
gDefault{|(,,,,,,)|} fa fb fc fd fe ff fg		= (fa,fb,fc,fd,fe,ff,fg)
gDefault{|(,,,,,,,)|} fa fb fc fd fe ff fg fh	= (fa,fb,fc,fd,fe,ff,fg,fh)
gDefault{|(->)|} fa fb	    				    = const fb
gDefault{|Dynamic|}		    				    = dynamic 42
gDefault{|Maybe|} fa	    				    = Nothing

gDefault{|HtmlTag|}		    				    = Html ""
gDefault{|Map|} fa fb                           = newMap

//SCARY BUG: When 'Map' is derived programs segfault when used in 'update' task on a shared source
derive gDefault Either, MaybeError, /*Map,*/ JSONNode, Timestamp, Timespec

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|}
