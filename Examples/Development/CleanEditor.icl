implementation module CleanEditor

import iTasks, Text
import qualified Data.Map
import projectManager
import System.FilePath

import SmallUtil, IDE_State

derive class iTask FileError

openFileSelectorAndEdit :: (ReadOnlyShared (TaskList Void)) -> Task Void
openFileSelectorAndEdit ts
	=				get_IDE_State
	>>= \state ->	selectFileInPath state.projectPath (\_ -> True) <<@ Window
	>>= \(path,r)-> if (isNothing r)
						(return Void)
						(launchEditorAndAdministrate (fromJust r) ts)

openEditorOnFiles :: [FileName] (ReadOnlyShared (TaskList Void)) -> Task Void
openEditorOnFiles [] 		ts =	return Void
openEditorOnFiles [f:fs]	ts =	launch (editor f ts) ts >>| openEditorOnFiles fs ts 


saveAll :: [FileName]  -> Task Void
saveAll [] 
	= return Void
saveAll [name:names]
	=				get (sharedStore name "")				// read out latest content of the editor from the internal store
	>>= \content ->	set content (externalFile name)			// and store it in the actual file			
	>>|				saveAll names 						


launchEditorAndAdministrate :: FileName (ReadOnlyShared (TaskList Void)) -> Task Void
launchEditorAndAdministrate fileName ts
	=				get_IDE_State
	>>= \state ->	if (isMember fileName state.openedFiles)
						(return Void)										// file already open in an editor
						(			addFileToAdmin fileName 
						>>|			launch (editor fileName ts) ts @ const Void  
						)

closeEditorAndAdministrate :: FileName -> Task Void
closeEditorAndAdministrate fileName
	=				removeFileFromAdmin fileName

// editor

:: Replace	 =	{ search  	:: String
				, replaceBy :: String
				}
derive class iTask Replace

initReplace =	{ search = ""
				, replaceBy = "" 
				}
editor :: FileName (ReadOnlyShared (TaskList Void)) -> Task Void
editor fileName ts = editor` (externalFile fileName)
where
	editor` file	
		=   			get file
		>>= \content ->	let copy = sharedStore fileName content in editor`` copy
	where
		editor`` copy 
			=  		(parallel (Title fileName`)	
						[ (Embedded, editFile fileName copy)
						, (Embedded, replace initReplace copy)
						]  @ const Void ) // <<@ AfterLayout (uiDefSetDirection Horizontal)
			>>*	 	[ OnAction  ActionClose 		 				   			(always (closeEditorAndAdministrate fileName))
					, OnAction (Action ("File/Close " +++ fileName`) []) 		(always (closeEditorAndAdministrate fileName))
					, OnAction (Action ("File/Save " +++ fileName`) []) 		(always (save copy >>| editor` file))
					, OnAction (Action ("File/Save As...") []) 		   			(always (saveAs copy >>| editor` file))
					, OnAction (Action ("File/Revert " +++ fileName`) []) 		(always (editor` file))
					, OnAction (Action ("File/Open " +++ other fileName`) [])	(ifCond isIclOrDcl (launchEditorAndAdministrate fileName ts))
	
//							, OnAction (Action ("Project/Set Project/" +++ noSuffix +++ " (.prj)")) 
//																			  (const isIcl)
//																			  (const (storeNewProject noSuffix initialPath // to fix
//																			  				 >>| editor` file )) 
							]

		fileName`	= dropDirectory fileName
		noSuffix 	= RemoveSuffix fileName

		save copy
			=				get copy
			>>= \content -> set	content file		

		saveAs copy
			=				get copy
			>>= \content -> get_IDE_State
			>>= \state ->	storeFileInPath state.projectPath fileName content <<@ Window

		other fileName
		| equal_suffix ".icl" fileName = (RemoveSuffix fileName) +++ ".dcl"
		| equal_suffix ".dcl" fileName = (RemoveSuffix fileName) +++ ".icl"
		= ""

		isIclOrDcl	= isIcl || equal_suffix ".dcl" fileName 
		isIcl  		= equal_suffix ".icl" fileName
											
editFile :: String (Shared String) (SharedTaskList Void) -> Task Void
editFile fileName sharedFile _
	=	(updateSharedInformation Void [UpdateWith toV fromV] sharedFile  @ const Void) <<@ noHints
where
	fromV _ (CleanCode text)	= text
	toV text 					= CleanCode text

replace cmnd sharedFile _ = noReplace cmnd 
where
	noReplace :: Replace -> Task Void 
	noReplace cmnd 
		=		actionTask
 			>>*	[ OnAction (Action "File/Replace" []) (always (showReplace cmnd))
				]

	showReplace :: Replace -> Task Void 
	showReplace cmnd
		=		updateInformation "Replace:" [] cmnd // <<@ Window
 			>>*	[ OnAction (Action "Replace" []) (hasValue substitute)
 				, OnAction (Action "Cancel" [])  (always (noReplace cmnd))
 				]
 			
 	substitute cmnd =	update (replaceSubString cmnd.search cmnd.replaceBy) sharedFile 
 						>>| showReplace cmnd

noHints = AfterLayout (tweakControls (\controls -> [(c,'Data.Map'.del VALID_ATTRIBUTE ('Data.Map'.del HINT_ATTRIBUTE m)) \\ (c,m) <- controls]))

