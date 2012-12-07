implementation module SmallUtil

/* Small utility functions...
*/

import iTasks 

actionTask :: Task Void
actionTask = viewInformation Void [] Void

launch :: (Task a) (ReadWriteShared (TaskList a) Void) -> Task Void | iTask a
launch task ts = appendTask Embedded (const task) ts @ const Void

// tiny util

always :: (a -> Bool)
always 	= const True

never :: (a -> Bool)
never 	= const False

hasValue :: (TaskValue a) -> Bool
hasValue (Value _ _) = True
hasValue _ = False

getValue :: (TaskValue a) -> a
getValue (Value v _) = v

ifValue :: (a -> Bool) (TaskValue a) -> Bool
ifValue pred (Value v _) = pred v
ifValue _ _ = False

ifStable :: (TaskValue a) -> Bool
ifStable (Value v Stable) = True
ifStable _ = False

