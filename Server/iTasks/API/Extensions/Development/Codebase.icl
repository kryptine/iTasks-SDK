implementation module iTasks.API.Extensions.Development.Codebase
import iTasks
import System.File, System.Directory,StdFile
import iTasks.API.Extensions.CodeMirror

derive class iTask Extension

codeBaseFromFiles :: [FilePath] -> Task CodeBase
codeBaseFromFiles paths = accWorld (getFilesInDir paths ["icl"] False)

navigateCodebase :: CodeBase -> Task (FilePath,ModuleName)
navigateCodebase codebase = navigate codebase

editCleanModule :: (FilePath,ModuleName) Extension -> Task ()
editCleanModule (path,fileName) Icl = openFile (path,fileName +++ ".icl") @! ()
editCleanModule (path,fileName) Dcl = openFile (path,fileName +++ ".dcl") @! ()



navigate tree = enterChoice [Att (Title "Select File"), Att IconEdit] [ChooseWith (ChooseFromTree (\list _ -> toChoiceTree list))] (treeToList tree [])
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
	treeToList [] 								      dirs = []
	treeToList [(path,[Leaf file:files]):tree] 	 	  dirs = [(path,dirs,file): treeToList [(path,files)] dirs] ++ treeToList tree []
	treeToList [(path,[Node dir childs :files]):tree] dirs = treeToList [(path,childs)] (dirs++[dir]) ++ treeToList [(path,files)] dirs ++ treeToList tree []
	treeToList [_:tree] 				      		  dirs = treeToList tree []

:: FileExtension :== String

getFilesInDir :: [FilePath] [FileExtension] !Bool !*World -> ([(FilePath,[TreeNode FilePath])],*World)
getFilesInDir [] extensions showExtension w = ([],w)
getFilesInDir [path:paths] extensions showExtension w
# (treeFiles,w)	= getTree (takeDirectory path) [dropDirectory path] w
# (ntrees,w)	= getFilesInDir paths extensions showExtension w
= ([(takeDirectory path,treeFiles):ntrees],w)
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
    | isEmpty extensions || isMember (snd (splitExtension fileName)) extensions
        # (treeNodes,w)         = getTree absolutePath fileNames w
		# name 					= if showExtension fileName (dropExtension fileName)
        = ([Leaf name:treeNodes],w)    
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

openFile (path,fileName) 
	=					openAndReadFile (path </> fileName)
	>>= \content ->		updateInformation fileName [] 
							(codeMirrorEditlet 	{ configuration = [ CMLineNumbers True
																  , CMMode "Haskell"
																  , CMDragDrop True
																  ] 			// [CodeMirrorConfiguration]
												, position		= 0				// cursor position
												, selection 	= Nothing		//!Maybe (Int,Int)
												, source		= content
												} []) 
where
	openAndReadFile  :: FilePath -> Task String
	openAndReadFile fileName
		=	accWorld (myfopen  fileName) 
	where
		myfopen fileName world 
		# (mbError,world) = readFile fileName world
		| isError mbError = (toString (fromError mbError),world)
		= (fromOk mbError,world)	