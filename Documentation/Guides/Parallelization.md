# Parallelization #

## Introduction ##
This guide provides insights on how to write iTasks programs that execute on multiple processor cores/processors/machines.

## Parallel primitives ##

### Distributed engine ###
The distributed engine is enabled by default and can be configured using:

- `EngineOptions.distributed :: Maybe Int` (`--distributed PORT`)

   This option either disables the distributed engine (`Nothing`) or enables it and offers distributed communication via the given port number.

- `EngineOptions.distributedChild :: Bool` (`--distributedChild`)

   This option starts the process in child mode.
   In child mode, the engine does not start any tasks except for the tasks required to function as a distributed iTasks node.

### Shared Data Sources ###
SDSs of different processes can be accessed using the `remoteShare` primitive:

```Clean
:: SDSShareOptions =
	{ domain :: !String
	, port   :: !Int
	}
remoteShare :: (sds p r w) SDSShareOptions -> SDSRemoteSource p r w | RWShared sds
```

### Tasks ###
Executing tasks on a different machine can be done using the `asyncTask` primitive:

```Clean
asyncTask :: !String !Int !(Task a) -> Task a | iTask a
```

There are no limitations to what you can execute on a different machine.
Do keep in mind that if you want to access SDSs from the parent, they need to be remote again.
The UI of the task is presented on the parent server.
Also, if you want to do a blocking calculation, make sure it is evaluated on the client and not already on the server.
To make sure this happen you can wrap the task:

```Clean
task = asyncTask
	"localhost"
	8000
	(return () >>- \_->return (somethingBlocking 42))
```

## Troubleshooting ##

When encountering problems ask yourself the following questions first:

- Are the correct project file options set? (`Link.GenerateSymbolTable` and `Application.Profile.DescExL`)
- Are the binaries of the server and the client compiled from the exact same source?
