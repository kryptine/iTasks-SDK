implementation module iTasks.API.Extensions.Development.Codebase
import iTasks
import System.File, System.Directory, Text, StdFile
import iTasks.API.Extensions.CodeMirror

derive class iTask SourceTree, ModuleType, Extension
instance == Extension where (==) x y = x === y

instance toString Extension
where
	toString Dcl = ".dcl"
	toString Icl = ".icl"

codeBaseFromEnvironment :: Environment -> Task CodeBase
codeBaseFromEnvironment paths
    =   allTasks [ accWorld (findModulesInDir path)
                 @ (\modules -> {SourceTree|rootDir=path,modules=modules,moduleFiles=[]})
                 \\ path <- paths]

navigateCodebase :: CodeBase -> Task (FilePath,ModuleName,ModuleType)
navigateCodebase codebase
    = enterChoice () [ChooseWith (ChooseFromTree group)] (modulesOf codebase)
where
    modulesOf codebase
        = flatten [[(rootDir,modName,modType) \\ (modName,modType) <- modules] \\ {SourceTree|rootDir,modules} <- codebase]

    group modules expanded = foldl insert [] modules
    where
	    insert nodeList (i,m=:(rootDir,modName,modType)) = insert` modName (split "." modName) nodeList
        where
    	    insert` wfpath [] nodeList = nodeList
		    insert` wfpath [title] nodeList = nodeList ++ [{ChoiceTree|label=title,icon=Just (icon modType),value=ChoiceNode i,type=LeafNode}]
		    insert` wfpath path=:[nodeP:pathR] [node=:{ChoiceTree|label=nodeL}:nodesR]
		    	| nodeP == nodeL	= [{ChoiceTree|node & type = ifExpandedChoice i expanded (insert` wfpath pathR (choiceTreeChildren node))}:nodesR]
		    	| otherwise			= [node:insert` wfpath path nodesR]
		    insert` wfpath path=:[nodeP:pathR] []
                = [{ChoiceTree|label=nodeP,icon=Nothing,value=GroupNode wfpath, type= ifExpandedGroup wfpath expanded (insert` wfpath pathR [])}]
		    insert` wfpath path [node:nodesR] = [node:insert` wfpath path nodesR]

        icon MainModule = "mainmodule"
        icon AuxModule = "auxmodule"

listFilesInCodeBase :: CodeBase -> [CleanFile]
listFilesInCodeBase codeBase
    = flatten [	[(rootDir, modName, Icl) \\ (modName,_)         <- modules] ++
    			[(rootDir, modName, Dcl) \\ (modName,AuxModule) <- modules]
	    	  \\ {SourceTree|rootDir,modules} <- codeBase]

    //TODO Also add dcl files

cleanFilePath :: CleanFile -> FilePath
cleanFilePath (baseDir,modName,ext) = foldl (</>) baseDir (split "." modName) +++ toString ext

getModuleType :: ModuleName CodeBase -> Maybe ModuleType
getModuleType modName [] = Nothing
getModuleType modName [{SourceTree|modules}:ts] = maybe (getModuleType modName ts) Just (search modules)
where
    search [] = Nothing
    search [(m,t):ms]
        | modName == m  = Just t
                        = search ms

codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]
codeBaseToCleanModuleNames codeBase
    = flatten [[(foldl (</>) rootDir (split "." modName), modName) \\ (modName,modType) <- modules] \\ {SourceTree|rootDir,modules} <- codeBase]

treeToList :: CodeBase [FilePath] -> [(FilePath,[FilePath],FilePath)]
treeToList [] 								      dirs = []
treeToList [{SourceTree|rootDir,moduleFiles=[Leaf file:files]}:tree] dirs = [(rootDir,dirs,file): treeToList [{SourceTree|rootDir=rootDir,moduleFiles=files,modules=[]}] dirs] ++ treeToList tree []
treeToList [{SourceTree|rootDir,moduleFiles=[Node dir childs :files]}:tree] dirs = treeToList [{SourceTree|rootDir=rootDir,moduleFiles=childs,modules=[]}] (dirs++[dir]) ++ treeToList [{SourceTree|rootDir=rootDir,moduleFiles=files,modules=[]}] dirs ++ treeToList tree []
treeToList [_:tree] 				      		  dirs = treeToList tree []

findModulesInDir :: !FilePath !*World -> ([(ModuleName,ModuleType)],*World)
findModulesInDir path w
    # (filesInDir,w) = getFilesInPath path w
    = find [] [path </> file \\ file <- filesInDir] w
where
    find modBase [] w = ([],w)
    find modBase [f:fs] w
        # (mbInfo,w)                = getFileInfo f w
        | isError mbInfo            = find modBase fs w
        | (fromOk mbInfo).directory
            # (filesInDir,w)        = getFilesInPath f w
            # (subModules,w)        = find [dropExtension (dropDirectory f):modBase] [f </> file \\ file <- filesInDir] w
            # (fsModules,w)         = find modBase fs w
            = (subModules ++ fsModules,w)
        # (fileName,ext)            = splitExtension (dropDirectory f)
        # (fsModules,w)             = find modBase fs w
        | ext == "icl"
            = (addModule (join "." (reverse [fileName:modBase])) False fsModules, w)
        | ext == "dcl"
            = (addModule (join "." (reverse [fileName:modBase])) True fsModules, w)
        = (fsModules,w)

    addModule modName isAux []
        = [(modName,if isAux AuxModule MainModule)]
    addModule modName isAux [(m,MainModule):ms]
        | modName == m && isAux = [(m,AuxModule):ms]
                                = [(m,MainModule):addModule modName isAux ms]
    addModule modName isAux [(m,type):ms]
        | modName == m          = [(m,type):ms]
                                = [(m,type):addModule modName isAux ms]

//UITZOEKEN: Protocol analysis

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

editCleanModule :: Bool CleanModule -> Task CodeMirror
editCleanModule mode ((path,fileName),ext) = openEditor mode (path,fileName +++ toString ext)

openEditor True (path,fileName)
	=					importTextFile (path </> fileName)
	>>- \content ->		withShared (initCleanEditor False content)
						(\config -> updateSharedInformation fileName [UpdateWith
																		(\cm -> codeMirrorEditlet cm [])
																		(\_ (Editlet value _ _) -> value) 
																	 ] config)
openEditor False (path,fileName)
	=					importTextFile (path </> fileName)
	>>- \content ->		withShared (initCleanEditor True content)
						(\config -> viewSharedInformation fileName [ViewWith
																		(\cm -> codeMirrorEditlet cm []) 
																	 ] config)

updateCleanEditor :: (Shared CodeMirror) FilePath ModuleName Extension -> Task CodeMirror
updateCleanEditor mirror base moduleName ext
=						importTextFile (foldl (</>) base (split "." moduleName) +++ toString ext)
	>>- \content ->		upd (\mir -> {mir & source = content}) mirror
	>>|					updateSharedInformation moduleName [UpdateWith  (\cm -> codeMirrorEditlet cm [])
																	  (\_ (Editlet value _ _) -> value)
														 ] mirror

viewCleanEditor :: (Shared CodeMirror) CleanModule -> Task CodeMirror
viewCleanEditor shared ((path,fileName),ext)
=						importTextFile (path </> fileName +++ toString ext)
	>>- \content ->		viewSharedInformation fileName [ViewWith
																		(\cm -> codeMirrorEditlet cm [])
																	 ] shared

initCleanEditor :: Bool String -> CodeMirror
initCleanEditor mode content
	=   { configuration = [ CMLineNumbers True
						  , CMMode "haskell"
						  , CMDragDrop True
		 				  , CMReadOnly mode
		 				  , CMAutofocus True
						  ] 			// [CodeMirrorConfiguration]
		 , position		= 0				// cursor position
		 , selection 	= Nothing		//!Maybe (Int,Int)
		 , highlighted	= []
		 , source		= content
		 }


