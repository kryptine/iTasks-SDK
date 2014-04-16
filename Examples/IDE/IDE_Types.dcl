definition module IDE_Types

import iTasks  
import iTasks.API.Extensions.Development.Codebase

:: IDE_Status = { openedFiles	:: [CleanModule]
			    , codeBase		:: CodeBase		    
			    }

derive class iTask IDE_Status

IDE_Status :: (Shared IDE_Status)
