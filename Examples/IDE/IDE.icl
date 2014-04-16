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
	editModules [module:modules]  	= editCleanModule module >>| editModules modules


test list
	= 					codeBaseFromEnvironment myEnv
	>>= \codeBase ->	searchForIdentifier SearchIdentifier True "test" (idePath,"IDE") codeBase
	>>= \result ->		viewInformation "result" [] result
	

//Start w = startEngine (test []) w

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
									-> appendTask Embedded (\_ -> cleanEditor ((filePath,moduleName),Icl)) list))
							, OnAction  (Action "/Open .dcl" [ActionKey (unmodified KEY_ENTER)])
								(ifValue isJust (\(Just (filePath,moduleName)) 
									-> appendTask Embedded (\_ -> cleanEditor ((filePath,moduleName),Dcl)) list))
							]
			@? const NoValue


cleanEditor ((filePath,moduleName),ext) 
	=   editCleanModule ((filePath,moduleName),ext)  <<@ Title (moduleName +++ toString ext) 
		>&>
//		viewSharedInformation "Selected" []  
			\mirror -> 	viewSharedInformation "Selected" [ViewWith (getSelection o fromJust)] mirror 
			>>*			[OnAction (Action "Search" []) (ifValue isJust (\mirror -> searchFor (getSelection (fromJust mirror))))]
		  

searchFor identifier
	= 					codeBaseFromEnvironment myEnv
	>>= \codeBase ->	searchForIdentifier SearchIdentifier True identifier (idePath,"IDE") codeBase
	>>= \result ->		viewInformation "result" [] result



getSelection :: CodeMirror -> Identifier
getSelection {position,selection=Nothing,source} =  "nothing"
getSelection {position,selection=Just (begin,end),source}
| begin == end =  "zero"
= source%(begin,end)


