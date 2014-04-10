module IDE
 
import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.Development.CleanCode
 
import  System.File
 
cleanPath 	:==  "C:\\Users\\rinus\\Desktop\\Clean_2.2"
idePath 	:==  cleanPath </> "iTasks-SDK\\Examples\\IDE"
stdEnv   	:==	 cleanPath  </> "Libraries\\StdEnv"
myEnv		:== [idePath,stdEnv]
 
//Start w  =  startEngine IDE_Dashboard w
Start w = startEngine (startWork []) w

startWork list
	= (chooseFile myEnv ["icl","dcl"] >&> workOnFiles) <<@ (ArrangeWithSideBar 0 LeftSide 200) <<@ FullScreen

workOnFiles :: (ReadOnlyShared (Maybe (FilePath,FilePath))) -> Task ()
workOnFiles sel 
	= parallel () [
		(Embedded,addSelectedFile sel)
	  ] [] <<@ ArrangeWithTabs @! ()
where
	addSelectedFile sel list
		= watch sel >^* [OnAction  (Action "/Start Editor" [ActionKey (unmodified KEY_ENTER)])
							(ifValue isJust (\(Just v) -> appendTask Embedded (\_ -> (openFile v <<@ (Title (snd v)))) list))]
		@? const NoValue

import iTasks.API.Extensions.CodeMirror, StdFile
 
openFile :: (FilePath,FilePath) -> Task String //(Editlet CodeMirror [CodeMirrorDiff])
openFile (path,fileName) 
	= 				openAndReadFile (path </> fileName)
	>>- viewInformation "TEST" [ViewWith Note]
	
	/*
	>>= \content -> updateInformation fileName [] 
							(codeMirrorEditlet 	{ configuration = [CMLineNumbers True] 			// [CodeMirrorConfiguration]
												, position		= 0				// cursor position
												, selection 	= Nothing		//!Maybe (Int,Int)
												, source		= content
												} []) 


	*/
where
	openAndReadFile  :: FilePath -> Task String
	openAndReadFile fileName
		=	accWorld (myfopen  fileName) 
	where
		myfopen fileName world 
		# (mbError,world) = readFile fileName world
		| isError mbError = (toString (fromError mbError),world)
		= (fromOk mbError,world)	
	
 

