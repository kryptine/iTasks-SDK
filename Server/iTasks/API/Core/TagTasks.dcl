definition module iTasks.API.Core.TagTasks

import Text.JSON
import GenEq
import iTasks.Framework.Task

class Tag a 
	| JSONEncode{|*|}
	, JSONDecode{|*|} a
	
tagStr :: t -> String | Tag t

(<<:) infixl 1 :: (Task a) t -> Task a | iTask a & Tag t

(>>:) infixl 1 :: t (Task a) -> Task a | iTask a & Tag t
