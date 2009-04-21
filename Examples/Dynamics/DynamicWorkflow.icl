module DynamicWorkflow


// WARNING: this example iTask application will only work with the "Enable dynamics" project option set


import iTasks

import DynamicIO

Start world = startEngine coffeemachineExample world 

coffeemachineExample :: [Workflow]
coffeemachineExample = [{ name 		= "Examples/Dynamics/ReadDynamic"
						, label 	= "ReadDynamic"
						, roles 	= []
						, mainTask 	= readWorkflow
						}]


readWorkflow :: Task Void
readWorkflow
		=						editTask "enter filename" ""
			>>= \filename ->	readDynamicTask filename
			>>= \(ok,task) ->	if ok (mytask task) (return Void)
where	
	mytask :: (Task Void) -> Task Void
	mytask task = task	
	



