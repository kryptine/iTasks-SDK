implementation module Incidone.RoleBased.PartnerTasks

import iTasks
import Incidone.OP.Conversions
import Incidone.OP.ContactManagementTasks
import Incidone.Util.TaskPatterns, Incidone.ActionManagementTasks

managePartnerActions :: [Workspace -> Task ()]
managePartnerActions = [welcome,myactions]
where
    welcome _
        = viewSharedInformation (Title "Welcome") [ViewAs (\u -> "Welcome "+++toString u)] currentUser @! ()

    myactions _
        =   get currentUser @ userContactNo
        >>- maybe (return ()) (manageContactActions False)

