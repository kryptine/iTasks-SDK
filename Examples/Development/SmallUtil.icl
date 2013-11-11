implementation module SmallUtil

/* Small utility functions...
*/

import iTasks 

actionTask :: Task Void
actionTask = viewInformation Void [] Void

launch :: (Task a) (SharedTaskList a) -> Task Void | iTask a
launch task ts = appendTask Embedded (const task) ts @ const Void
