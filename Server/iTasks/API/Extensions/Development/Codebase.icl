implementation module iTasks.API.Extensions.Development.Codebase
import iTasks
import System.File, System.Directory,StdFile
import iTasks.API.Extensions.CodeMirror

derive class iTask SourceTree, LibraryTree, ApplicationTree, Extension
instance == Extension where (==) x y = x === y

instance toString Extension
where
	toString Dcl = ".dcl"
	toString Icl = ".icl"

codeBaseFromEnvironment :: Environment -> Task CodeBase
codeBaseFromEnvironment paths 
    = accWorld (getFilesInDir paths ["icl"] False)
    @ map (\(rootDir,moduleFiles) -> LibraryTree {LibraryTree|rootDir=rootDir,moduleFiles=moduleFiles})

navigateCodebase :: CodeBase -> Task CleanModuleName
navigateCodebase codebase = navigate codebase
where
	navigate tree = enterChoice [Att (Title "Select File"), Att IconEdit] [ChooseWith (ChooseFromTree toChoiceTree)] (treeToList tree [])
					@? adjust
	where
		toChoiceTree :: [(Int,(FilePath,[FilePath],FilePath))] [ChoiceTreeValue] -> [ChoiceTree FilePath]
		toChoiceTree [] expanded = []
		toChoiceTree [(i,(path,[],fileName)):next] expanded
		= [{label = fileName, icon = Nothing, value = ChoiceNode i, type = LeafNode}:toChoiceTree next expanded]
		toChoiceTree [(i,(path,[dir:dirs],fileName)):next] expanded
		= [{label = dir, icon = Nothing, value = GroupNode dir,
            type = ifExpandedGroup dir expanded (toChoiceTree inDir` expanded)}:toChoiceTree outDir expanded]
		where
			(inDir,outDir) = span (\(_,(_,dirs,_)) -> if (not (isEmpty dirs)) (hd dirs == dir) False) next
			inDir` = [(i,(path,dirs,fileName)):[(j,(path,tl dirs,name)) \\ (j,(path,dirs,name)) <- inDir]]		
	
		adjust (Value (path,dirs,fileName) stab)
		| fileName == "" = NoValue
		= Value (foldl (</>) path dirs,fileName) stab
		adjust NoValue   = NoValue

codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]
codeBaseToCleanModuleNames codeBase 
= [(foldl (</>) path dirs,fileName) \\ (path,dirs,fileName) <- treeToList codeBase []]

treeToList :: CodeBase [FilePath] -> [(FilePath,[FilePath],FilePath)]
treeToList [] 								      dirs = []
treeToList [LibraryTree {LibraryTree|rootDir,moduleFiles=[Leaf file:files]}:tree] dirs = [(rootDir,dirs,file): treeToList [LibraryTree {LibraryTree|rootDir=rootDir,moduleFiles=files}] dirs] ++ treeToList tree []
treeToList [LibraryTree {LibraryTree|rootDir,moduleFiles=[Node dir childs :files]}:tree] dirs = treeToList [LibraryTree {LibraryTree|rootDir=rootDir,moduleFiles=childs}] (dirs++[dir]) ++ treeToList [LibraryTree{LibraryTree|rootDir=rootDir,moduleFiles=files}] dirs ++ treeToList tree []
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
= ([],w)


openEditor True (path,fileName) 
	=					openAndReadFile (path </> fileName)
	>>= \content ->		withShared (initCleanEditor False content)
						(\config -> updateSharedInformation fileName [UpdateWith 
																		(\cm -> codeMirrorEditlet cm [])
																		(\_ (Editlet value _ _) -> value) 
																	 ] config) 
openEditor False (path,fileName) 
	=					openAndReadFile (path </> fileName)
	>>= \content ->		withShared (initCleanEditor True content)
						(\config -> viewSharedInformation fileName [ViewWith 
																		(\cm -> codeMirrorEditlet cm []) 
																	 ] config) 
editCleanModule :: Bool CleanModule -> Task CodeMirror
editCleanModule mode ((path,fileName),ext) = openEditor mode (path,fileName +++ toString ext)// @! ()



updateCleanEditor :: (Shared CodeMirror) CleanModule -> Task CodeMirror
updateCleanEditor mirror ((path,fileName),ext) 
=						openAndReadFile (path </> fileName +++ toString ext)
	>>= \content ->		upd (\mir -> {mir & source = content}) mirror 
	>>|					updateSharedInformation fileName [UpdateWith  (\cm -> codeMirrorEditlet cm [])
																	  (\_ (Editlet value _ _) -> value) 
														 ] mirror

viewCleanEditor :: (Shared CodeMirror) CleanModule -> Task CodeMirror
viewCleanEditor shared ((path,fileName),ext) 
=						openAndReadFile (path </> fileName +++ toString ext)
	>>= \content ->		viewSharedInformation fileName [ViewWith 
																		(\cm -> codeMirrorEditlet cm []) 
																	 ] shared

openAndReadFile  :: FilePath -> Task String
openAndReadFile fileName
	=	accWorld (myfopen  fileName) 
where
	myfopen fileName world 
	# (mbError,world) = readFile fileName world
	| isError mbError = (toString (fromError mbError),world)
	= (fromOk mbError,world)	

initCleanEditor :: Bool String -> CodeMirror
initCleanEditor mode content
	=   { configuration = [ CMLineNumbers True
						  , CMMode "haskell"
						  , CMDragDrop True
		 				  , CMReadOnly mode
						  ] 			// [CodeMirrorConfiguration]
		 , position		= 0				// cursor position
		 , selection 	= Nothing		//!Maybe (Int,Int)
		 , highlighted	= []
		 , source		= content
		 }


