definition module iTasks.API

import
     // Predefined types with their generic instances
        iTasks.API.Core.Types

     //  Basic tasks
     ,  iTasks.API.Extensions.Process // Tasks for integration with other systems

     ,  iTasks.API.Common.InteractionTasks   // Tasks for interaction with users
     ,  iTasks.API.Common.DBTasks            // convenience wrapper functions for databases with multiple values of type a
     ,  iTasks.API.Common.ImportTasks        // tasks for importing external data
     ,  iTasks.API.Common.ExportTasks        // tasks for exporting data

     //  Task combinators
     ,  iTasks.API.Common.TaskCombinators  // Set of derived useful iTask combinators
