definition module iTasks.API

import
     //  System data
        iTasks.API.Core.SystemData
     ,  iTasks.API.Core.SystemTypes

     //  Basic tasks
     ,  iTasks.API.Core.CoreTasks            // Core basic tasks
     ,  iTasks.API.Core.OptimizedCoreTasks   // Optimized core basic tasks
     ,  iTasks.API.Common.InteractionTasks   // Tasks for interaction with users
     ,  iTasks.API.Common.DBTasks            // convenience wrapper functions for databases with multiple values of type a

     ,  iTasks.API.Common.ImportTasks        // tasks for importing external data
     ,  iTasks.API.Common.ExportTasks        // tasks for exporting data  
     ,  iTasks.API.Core.IntegrationTasks     // Tasks for integration with other systems

     //  Task combinators
     ,  iTasks.API.Core.CoreCombinators      // The core iTask combinators
     ,  iTasks.API.Common.CommonCombinators  // Set of derived useful iTask combinators

     //  Layout tuning
     ,  iTasks.API.Core.LayoutCombinators
