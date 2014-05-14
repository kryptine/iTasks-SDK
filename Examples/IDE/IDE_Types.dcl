definition module IDE_Types

import iTasks  
import iTasks.API.Extensions.Development.Codebase

:: IDE_Status =  { info 	 	 :: [EditorInfo]
			     , codeBase		 :: CodeBase
                 , codeLocations :: [FilePath]
			     }
:: EditorInfo =  { cleanModule	 :: CleanModule
				 , codeMirrir	 :: CodeMirror
				 , opened		 :: Bool
				 }


derive class iTask IDE_Status, EditorInfo


