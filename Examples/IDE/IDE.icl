module IDE
 
import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.Development.CleanCode
 
import System.Directory, System.File
 
 
//Start w  =  startEngine (enterChoice [Att (Title "Select File"), Att IconEdit] [] ["noot","mies"] >>= viewInformation "result" []) w
//Start w  =  startEngine (chooseFile >>= viewInformation "result" []) w
//Start w  =  startEngine (					accWorld (getFilesInDir filePath  [] /*["icl","dcl","prj"]*/) 
//							>>= \tree ->	viewInformation "result" [] (treeToList tree)) w
//Start w  =  startEngine (chooseFile >&> viewAndStart) w
//Start w  =  startEngine IDE_Dashboard w
Start w = startEngine (startWork []) w


toVoid :: (Task a) -> Task Void | iTask a
toVoid taska = taska @ (\_ -> Void)

IDE_Dashboard :: Task Void
IDE_Dashboard
	=  parallel Void
		[ (Embedded, toVoid o startWork)
		, (Embedded, toVoid o controlDashboard)
		, (Embedded, toVoid o manageWork)
		] [] <<@ ArrangeCustom layout <<@ FullScreen 
	>>* [OnValue (ifValue (\results -> isValue (snd (results !! 1))) (\_ -> return Void))]
where
	isValue (Value _ _) = True
	isValue _			= False

    layout {UISubUIStack|attributes,subuis=[startWork,dashBoard,manageWork:activeWork],size}
        = arrangeWithSideBar 0 LeftSide 260 {UISubUIStack|attributes=attributes,subuis=[startWork,mainArea],size=size}
    where
        mainArea = arrangeWithSideBar 0 TopSide 30 (toSubUIStack [dashBoard,workArea])
        workArea = arrangeWithSideBar 0 TopSide 200 (toSubUIStack [manageWork,tabsArea])
        tabsArea = arrangeWithTabs (toSubUIStack activeWork)
    layout stack = autoLayoutSubUIStack stack


controlDashboard list
	=	viewInformation "controlDashboard" [] "Nothing Yet"	
 
manageWork list
	=	viewInformation "manageWork" [] "Nothing to manage Yet"	 


cleanPath 	:==  "C:\\Users\\rinus\\Desktop\\Clean_2.2"
idePath 	:==  cleanPath </> "iTasks-SDK\\Examples\\IDE"
stdEnv   	:==	 cleanPath  </> "Libraries\\StdEnv"
myEnv		:== [idePath,stdEnv]

//startWork :: Task ClientPart
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
	= 				fileToString (path </> fileName)
	>>- viewInformation "TEST" [ViewWith Note]
	
	/*
	>>= \content -> updateInformation fileName [] 
							(codeMirrorEditlet 	{ configuration = [CMLineNumbers True] 			// [CodeMirrorConfiguration]
												, position		= 0				// cursor position
												, selection 	= Nothing		//!Maybe (Int,Int)
												, source		= content
												} []) 


	*/
fileToString  :: FilePath -> Task String
fileToString fileName
	=	accWorld (myfopen  fileName) 
where
	myfopen fileName world 
	# (mbError,world) = readFile fileName world
	| isError mbError = (toString (fromError mbError),world)
	= (fromOk mbError,world)	
	
 
filerSelected ::  Shared [(FilePath,[TreeNode FilePath])]
filerSelected = sharedStore "fileSelected" [] 

filesToSelect :: [(FilePath,[TreeNode FilePath])] -> ReadOnlyShared [(FilePath,[TreeNode FilePath])]
filesToSelect files = toReadOnly (mapRead (\_ -> files) filerSelected)

