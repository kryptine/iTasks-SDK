definition module iTaskCombinatorsExt

import iTasks

//	Useful derived combinators for iTasks:
stopTask		:: (Task a) -> Task a | iTask a
orTasksVert		:: [Task a] -> Task a | iTask a
yesOrNo			:: [HtmlTag] (Task a) (Task a) -> Task a | iTask a
OK				:: Task Void