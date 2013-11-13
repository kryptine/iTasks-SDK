implementation module iTasks.API.Core.TagTasks

import StdString
import Text.JSON
from StdFunc import o
import iTasks
import iTasks.Framework.TaskState

tagStr :: t -> String | Tag t
tagStr x = (toString o toJSON) x

(<<:) infixl 1 :: (Task a) t -> Task a | iTask a & Tag t
(<<:) t ident = updateIdAttr ident t

(>>:) infixl 1 :: t (Task a) -> Task a | iTask a & Tag t
(>>:) ident t = updateIdAttr ident t

updateIdAttr :: t (Task a) -> Task a | iTask a & Tag t
updateIdAttr ident (Task t) = Task eval
where
	eval event repOpts tt=:(TCDestroy _) iworld = t event repOpts tt iworld
	eval event repOpts tt iworld =
		case (t event repOpts tt iworld) of
		(ValueResult tv ti (TaskRep ui reps) tt2, world)
			= (ValueResult tv ti (TaskRep ui (map project reps)) tt2, world)
		x = x
	project t=:{TaskPart | tag = Nothing} = {t & tag = Just (toJSON ident)}
	project t = t
