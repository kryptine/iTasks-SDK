definition module iTaskCombinatorsExt

import iTasks

//	Useful derived combinators for iTasks:
stopTask		:: (Task a) -> Task a | iData a
orTasksVert		:: [Task a] -> Task a | iData a
yesOrNo			:: [HtmlTag] (Task a) (Task a) -> Task a | iData a
OK				:: Task Void