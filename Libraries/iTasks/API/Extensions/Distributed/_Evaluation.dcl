definition module iTasks.API.Extensions.Distributed._Evaluation

import iTasks

/*
 * Evaluate a remote task.
 *
 * @param Remote task.
 * @param Task that handles a value change.
 * @return Task result of remote task.
 */
evalRemoteTask :: (Task a) ((TaskValue a) -> Task ()) -> Task a | iTask a

proxyTask :: (RWShared () (TaskValue a) (TaskValue a)) (*IWorld -> *IWorld) -> (Task a) | iTask a

taskValueShare :: Int ->  RWShared () (TaskValue a) (TaskValue a) | iTask a
