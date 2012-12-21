implementation module SmallUtil

/* Small utility functions...
*/

import iTasks 

actionTask :: Task Void
actionTask = viewInformation Void [] Void

launch :: (Task a) (ReadWriteShared (TaskList a) Void) -> Task Void | iTask a
launch task ts = appendTask Embedded (const task) ts @ const Void
// tiny util

always :: (Task b) (TaskValue a) -> Maybe (Task b)
always taskb val = Just taskb

never :: (Task b) (TaskValue a) -> Maybe (Task b)
never taskb val	= Nothing

ifValue :: (a -> Bool) (a -> Task b) (TaskValue a) -> Maybe (Task b)
ifValue pred ataskb (Value a _) 
| pred a 	= Just (ataskb a)
| otherwise = Nothing

hasValue	:: (a -> Task b) 				(TaskValue a) -> Maybe (Task b)
hasValue ataskb (Value a _) = Just (ataskb a)
hasValue _ _ = Nothing


ifCond :: Bool (Task b) (TaskValue a) -> Maybe (Task b)
ifCond True taskb _ = Just taskb
ifCond False taskb _ = Nothing

ifStable :: (a -> Task b) (TaskValue a) -> Maybe (Task b)
ifStable ataskb (Value a True) = Just (ataskb a)
ifStable _ _ 				   = Nothing

ifUnstable :: (a -> Task b) (TaskValue a) -> Maybe (Task b)
ifUnstable ataskb (Value a False) = Just (ataskb a)
ifUnstable _ _ 				   = Nothing

