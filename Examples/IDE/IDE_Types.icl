implementation module IDE_Types
 
import iTasks
import iTasks.API.Extensions.Development.Codebase

derive class iTask IDE_Status

IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ openedFiles = []
			    						, codeBase 	  = []
			    						}
			     

