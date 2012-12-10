definition module CleanEditor

import iTasks

import IDE_State

openFileSelectorAndEdit 	:: (ReadOnlyShared (TaskList Void)) -> Task Void

openEditorOnFiles 			:: [FileName] (ReadOnlyShared (TaskList Void)) -> Task Void

launchEditorAndAdministrate :: FileName (ReadOnlyShared (TaskList Void)) -> Task Void

closeEditorAndAdministrate 	:: FileName -> Task Void

saveAll 					:: [FileName]  -> Task Void
											
