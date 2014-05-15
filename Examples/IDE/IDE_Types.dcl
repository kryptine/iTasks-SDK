definition module IDE_Types

import iTasks
import iTasks.API.Extensions.Development.Codebase

:: IDE_Status =  { codeBase		 :: CodeBase        //Cached shared structured with
                 , codeLocations :: [FilePath]      //File locations where clean code is on disk
                 , recentModules :: [CleanModule]   //Recently opened modules
			     }

derive class iTask IDE_Status

