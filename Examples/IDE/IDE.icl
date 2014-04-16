module IDE
 
import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.CodeMirror, StdFile
 
import FindDefinitions 

import IDE_Types

initIDE
	=					codeBaseFromEnvironment myEnv
	>>= \codeBase ->	upd (\status -> {status & codeBase = codeBase}) IDE_Status
	>>= \status ->		editModules status.openedFiles
where
	editModules [] 					= return ()
	editModules [module:modules]  	= editCleanModule module


test list
	= 					codeBaseFromEnvironment myEnv
	>>= \codeBase ->	searchForIdentifier SearchIdentifier False "map" (idePath,"IDE") codeBase
	>>= \result ->		viewInformation "result" [] result
	

Start w = startEngine (test []) w

Start w = startEngine (startWork []) w
where
	startWork list
		= (		(					initIDE 
					>>|				get IDE_Status 
					>>= \status -> 	navigateCodebase status.codeBase
				)
			>&> 
				workOnCode
		  ) <<@ (ArrangeWithSideBar 0 LeftSide 200 True) <<@ FullScreen

	workOnCode :: (ReadOnlyShared (Maybe (FilePath,FilePath))) -> Task ()
	workOnCode sel 
		= 	parallel () [(Embedded,\list -> addSelectedModule sel list)
						] [] <<@ ArrangeWithTabs @! ()
	where
		addSelectedModule sel list
			= watch sel >^* [ OnAction  (Action "/Open .icl" [ActionKey (unmodified KEY_ENTER)])
								(ifValue isJust (\(Just (filePath,moduleName)) 
									-> appendTask Embedded (\_ -> (editCleanModule ((filePath,moduleName),Icl)  <<@ (Title (moduleName +++ ".icl")))) list))
							, OnAction  (Action "/Open .dcl" [ActionKey (unmodified KEY_ENTER)])
								(ifValue isJust (\(Just (filePath,moduleName)) 
									-> appendTask Embedded (\_ -> (editCleanModule ((filePath,moduleName),Dcl)  <<@ (Title (moduleName +++ ".dcl")))) list))
							]
			@? const NoValue

 
