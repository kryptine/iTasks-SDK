definition module CleanEditor

import iTasks

import IDE_State

openFileSelectorAndEdit 	:: (SharedTaskList Void) -> Task Void

openEditorOnFiles 			:: [FileName] (SharedTaskList Void) -> Task Void

launchEditorAndAdministrate :: FileName (SharedTaskList Void) -> Task Void

closeEditorAndAdministrate 	:: FileName -> Task Void

saveAll 					:: [FileName]  -> Task Void
											
