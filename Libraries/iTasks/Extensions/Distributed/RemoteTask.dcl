definition module iTasks.Extensions.Distributed.RemoteTask

import iTasks
import iTasks.Extensions.Distributed._Formatter
from iTasks.Extensions.Distributed.Task import :: Domain

/*
 * Assign a task to a remote devie.
 */
remoteAssignTask :: !TaskAttributes (Task a) Domain -> Task a | iTask a
