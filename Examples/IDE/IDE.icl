module IDE
 
import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.Development.CleanCode
 
import System.Directory, System.File
 
filePath =  "C:\\Users\\rinus\\Desktop\\Clean_2.2\\iTasks-SDK\\Examples\\IDE"
 
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


//startWork :: Task ClientPart
startWork list
	= (chooseFile >&> viewAndStart) <<@ (ArrangeWithSideBar 1 BottomSide 200) 


viewAndStart :: (ReadOnlyShared (Maybe (FilePath,FilePath))) -> Task (Editlet CodeMirror [CodeMirrorDiff])
viewAndStart sel 
=	forever (
			viewSharedInformation [Att (Title "Show Selection"), Att IconView]  [ViewWith mbJust] sel 
		>>* [OnAction (Action "Start Editor" [ActionKey (unmodified KEY_ENTER)]) 
						(hasValue (\v -> openFile (fromJust v)))]
		)
where
	mbJust (Just v) = v
	mbJust _ = ("","")


import iTasks.API.Extensions.CodeMirror, StdFile
 
openFile :: (FilePath,FilePath) -> Task (Editlet CodeMirror [CodeMirrorDiff])
openFile (path,fileName) 
	= 				fileToString (path </> fileName)
	>>= \content -> updateInformation fileName [] 
							(codeMirrorEditlet 	{ configuration = [CMLineNumbers True] 			// [CodeMirrorConfiguration]
												, position		= 0				// cursor position
												, selection 	= Nothing		//!Maybe (Int,Int)
												, source		= content
												} []) 



fileToString  :: FilePath -> Task String
fileToString fileName
	=	accWorld (myfopen  fileName) 
where
	myfopen fileName world 
	# (mbError,world) = readFile fileName world
	| isError mbError = (toString (fromError mbError),world)
	= (fromOk mbError,world)	
	
:: FileExtension :== !String
 
filerSelected ::  Shared [(FilePath,[TreeNode FilePath])]
filerSelected = sharedStore "fileSelected" [] 

filesToSelect :: [(FilePath,[TreeNode FilePath])] -> ReadOnlyShared [(FilePath,[TreeNode FilePath])]
filesToSelect files = toReadOnly (mapRead (\_ -> files) filerSelected)

chooseFile :: Task (FilePath,FilePath)
chooseFile
	=					accWorld (getFilesInDir filePath  ["icl","dcl","prj","abc","o"]) 
		>>= \tree ->	enterChoice [Att (Title "Select File"), Att IconEdit] [ChooseWith (ChooseFromTree (\list _ -> toChoiceTree list))] (treeToList tree [])
		@? adjust
where
	toChoiceTree :: [(Int,(FilePath,[FilePath],FilePath))] -> [ChoiceTree FilePath]
	toChoiceTree []  							= []
	toChoiceTree [(i,(path,[],fileName)):next]
	= [{label = fileName, icon = Nothing, value = ChoiceNode i, type = LeafNode}:toChoiceTree next]
	toChoiceTree [(i,(path,[dir:dirs],fileName)):next]
	= [{label = dir, icon = Nothing, value = GroupNode dir, type = CollapsedNode (toChoiceTree inDir`)}:toChoiceTree outDir]
	where
		(inDir,outDir) = span (\(_,(_,dirs,_)) -> if (not (isEmpty dirs)) (hd dirs == dir) False) next
		inDir` = [(i,(path,dirs,fileName)):[(j,(path,tl dirs,name)) \\ (j,(path,dirs,name)) <- inDir]]		

	adjust (Value (path,dirs,fileName) stab)
	| fileName == "" = NoValue
	= Value (foldl (</>) path dirs,fileName) stab
	adjust NoValue   = NoValue

	treeToList :: [(FilePath,[TreeNode FilePath])] [FilePath] -> [(FilePath,[FilePath],FilePath)]
	treeToList [] 								 dirs = []
	treeToList [(path,[])] 						 dirs = []
	treeToList [(path,[Leaf file:files])] 		 dirs = [(path,dirs,file): treeToList [(path,files)] dirs]
	treeToList [(path,[Node dir childs :files])] dirs = treeToList [(path,childs)] (dirs++[dir]) ++ treeToList [(path,files)] dirs

getFilesInDir :: !FilePath [FileExtension] !*World -> ([(FilePath,[TreeNode FilePath])],*World)
getFilesInDir absolutePath extensions w
# (files,w)     = getFilesInPath absolutePath w
# (treeFiles,w)	= getTree absolutePath files w
= ([(absolutePath,treeFiles)],w)
where
    getTree absolutePath [] w   = ([],w)
    getTree absolutePath [fileName:fileNames] w
    # absoluteFileName          = absolutePath </> fileName
    # (mbInfo,w)                = getFileInfo absoluteFileName w
    | isError mbInfo            = getTree absolutePath fileNames w
    | (fromOk mbInfo).directory // file is directory name
        # (filesInDir,w)        = getFilesInPath absoluteFileName w
        # (dirNodes,w)          = getTree absoluteFileName filesInDir w
        # (filesNodes,w)		= getTree absolutePath fileNames w
        = case dirNodes of
            [] -> (filesNodes,w)
            _  -> ([Node fileName dirNodes:filesNodes],w)
    | isEmpty extensions ||    isMember (snd (splitExtension fileName)) extensions
        # (treeNodes,w)         = getTree absolutePath fileNames w
        = ([Leaf fileName:treeNodes],w)    
    = getTree absolutePath fileNames w    
        
getFilesInPath :: !FilePath !*World -> ([FilePath],!*World)
getFilesInPath path w
# (mbFiles,w)        = readDirectory path w
| isError mbFiles    = ([],w)
= ([name \\ name <- fromOk mbFiles | name <> "." && name <> ".."],w)
 
readDir :: !FilePath !*World -> ([FilePath],!*World)
readDir path w
# (mbInfo,w)                 = getFileInfo path w
| isError mbInfo             = ([],w)
| (fromOk mbInfo).directory = getFilesInPath path w
= ([],w)
 
test :: Task ()
test
    =   navigateCodebase []
    >&> \cs -> whileUnchanged cs
        \c -> case c of
            Nothing = viewInformation "Select a module first..." [] ()
            Just (d,m)  = viewCleanModule d m
