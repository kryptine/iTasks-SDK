definition module IDE_Types

import iTasks  
import iTasks.API.Extensions.Development.Codebase

:: IDE_Status = { openedFiles	:: [CleanModule]
			    , codeBase		:: CodeBase
                , codeLocations :: [FilePath]
			    }

derive class iTask IDE_Status


