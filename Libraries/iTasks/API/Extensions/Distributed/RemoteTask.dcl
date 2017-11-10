definition module iTasks.API.Extensions.Distributed.RemoteTask

import iTasks
import iTasks.API.Extensions.Distributed._Formatter
from iTasks.API.Extensions.Distributed.Task import :: Domain

/*
 * Assign a task to a remote devie.
 */
remoteAssignTask :: !TaskAttributes (Task a) Domain -> Task a | iTask a
