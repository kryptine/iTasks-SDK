implementation module iTasks.API.Extensions.Admin.ServerAdmin
import iTasks

manageServer :: Task ()
manageServer
    =   enterChoiceWithShared "Session instances" [] currentSessions
    -&&-
        enterChoiceWithShared "Persistent instances" [] currentProcesses
    @!  ()
