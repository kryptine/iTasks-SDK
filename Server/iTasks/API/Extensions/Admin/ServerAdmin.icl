implementation module iTasks.API.Extensions.Admin.ServerAdmin
import iTasks

manageServer :: Task Void
manageServer
    =   enterChoiceWithShared "Session instances" [] currentSessions
    -&&-
        enterChoiceWithShared "Persistent instances" [] currentProcesses
    @!  Void
