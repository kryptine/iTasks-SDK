module IDE
 
import iTasks
import iTasks.API.Extensions.Development.Codebase
//import iTasks.API.Extensions.Development.CleanCode
import iTasks.API.Extensions.CodeMirror, StdFile
import  System.File
 
// for the time being one has to set the search paths in the code

cleanPath 	:==  "C:\\Users\\rinus\\Desktop\\Clean_2.2"
idePath 	:==  cleanPath </> "iTasks-SDK\\Examples\\IDE"
stdEnv   	:==	 cleanPath </> "Libraries\\StdEnv"
itaskEnv	:==  cleanPath </> "iTasks-SDK\\Server"
myEnv		:== [idePath,itaskEnv,stdEnv]
 
:: IDE_Status = { openedFiles :: [(FilePath,ModuleName,Extension)]
			    }

derive class iTask IDE_Status

IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ openedFiles = []
			    						}			     
initIDE
	=				get IDE_Status
	>>= \status ->	editModules status.openedFiles
where
	editModules [] 							= return ()
	editModules [(path,name,ext):modules]  	= editCleanModule (path,name) ext



Start w = startEngine (startWork []) w
where
	startWork list
		= (		(codeBaseFromFiles myEnv >>= navigateCodebase)
			>&> 
				workOnCode
		  ) <<@ (ArrangeWithSideBar 0 LeftSide 200) <<@ FullScreen

	workOnCode :: (ReadOnlyShared (Maybe (FilePath,FilePath))) -> Task ()
	workOnCode sel 
		= 	parallel () [(Embedded,\list -> initIDE >>| addSelectedModule sel list)
						] [] <<@ ArrangeWithTabs @! ()
	where
		addSelectedModule sel list
			= watch sel >^* [ OnAction  (Action "/Open .icl" [ActionKey (unmodified KEY_ENTER)])
								(ifValue isJust (\(Just v) -> appendTask Embedded (\_ -> (editCleanModule v Icl <<@ (Title (snd v +++ ".icl")))) list))
							, OnAction  (Action "/Open .dcl" [ActionKey (unmodified KEY_ENTER)])
								(ifValue isJust (\(Just v) -> appendTask Embedded (\_ -> (editCleanModule v Dcl <<@ (Title (snd v +++ ".dcl")))) list))
							]
			@? const NoValue

 
