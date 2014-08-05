implementation module iTasks.API.Extensions.Development.Codebase
import iTasks
import System.File, System.Directory, Text, StdFile, Data.List
import iTasks.API.Extensions.CodeMirror

derive class iTask SourceTree, SourceTreeSelection, ModuleType, Extension
instance == Extension where (==) x y = x === y

instance toString Extension
where
	toString Dcl = ".dcl"
	toString Icl = ".icl"

rescanCodeBase :: CodeBase -> Task CodeBase
rescanCodeBase codebase
    =   allTasks [ accWorld (findModulesForTree tree)
                 @ (\modules -> {SourceTree|tree & modules=modules})
                 \\ tree <- codebase]

navigateCodebase :: CodeBase -> Task SourceTreeSelection
navigateCodebase codebase
    = enterChoice () [ChooseWith (ChooseFromTree (groupModules (sourceTreeRoots codebase)))] (modulesOf codebase)
where
    modulesOf codebase
        = flatten [[SelSourceTree name:[moduleSelection modName modType modPath \\ (modName,modType,modPath) <- modules]] \\ {SourceTree|name,modules} <- codebase]

    sourceTreeRoots codebase
        = flatten (map roots codebase)
    where
        roots {SourceTree|name,rootPath,subPaths=[]}  = [(name,rootPath)]
        roots {SourceTree|name,rootPath,subPaths}     = [(name,rootPath </> sub) \\sub <- subPaths]

    moduleSelection modName MainModule modPath = SelMainModule modName modPath
    moduleSelection modName AuxModule modPath = SelAuxModule modName modPath

    groupModules roots options expanded = sortByLabel (foldl insert [] options)
    where
        //Add a new source tree
	    insert nodeList (i,m=:(SelSourceTree name))
            = nodeList ++ [{ChoiceTree|label=name,icon=Just "sourcetree",value=ChoiceNode i, type = ifExpandedChoice i expanded []}]
        //Find the node that holds the tree to which this module belongs, and add it there
        insert nodeList (i,m) = insert` (sourceTreeOf m roots) (split "." (moduleName m)) (i,m) nodeList

        insert` Nothing _ _ nodeList = nodeList
        insert` _ _ _ [] = []
	    insert` (Just treeName) moduleSplit (i,m) [n=:{ChoiceTree|label}:ns]
            | label == treeName = [{ChoiceTree|n & type = case n.ChoiceTree.type of
                                        ExpandedNode nodes = ExpandedNode (insert`` moduleSplit (i,m) nodes)
                                        CollapsedNode nodes = CollapsedNode (insert`` moduleSplit (i,m) nodes)
                                   }:ns]
            | otherwise         = [n:insert` (Just treeName) moduleSplit (i,m) ns]
        where
            insert`` [] (i,m) nodeList = nodeList
            //Search
            insert`` path=:[nodeP:pathR] (i,m) [node=:{ChoiceTree|label=nodeL,value}:nodesR]
                | nodeP == nodeL
                    # type = ifExpandedChoice i expanded (insert`` pathR (i,m) (choiceTreeChildren node))
                    | pathR =:[]
                        = [{ChoiceTree|node & value = ChoiceNode i, icon = Just (moduleIcon m), type = type}:nodesR]
                    = [{ChoiceTree|node & type = type}:nodesR]
                | otherwise         = [node:insert`` path (i,m) nodesR]
		    insert`` path=:[nodeP:pathR] (i,m) []
                | pathR =:[]
                    = [{ChoiceTree|label=nodeP,icon=Just (moduleIcon m),value=ChoiceNode i, type= LeafNode}]
                | otherwise
                    = [{ChoiceTree|label=nodeP,icon=Nothing,value=GroupNode (moduleName m), type= ifExpandedGroup (moduleName m) expanded (insert`` pathR (i,m) [])}] 
        moduleName (SelMainModule name _) = name
        moduleName (SelAuxModule name _) = name

        modulePath (SelMainModule _ path) = path
        modulePath (SelAuxModule _ path) = path

        moduleIcon (SelMainModule _ _) = "mainmodule"
        moduleIcon (SelAuxModule _ _) = "auxmodule"

        sourceTreeOf m roots
            = case [name \\ (name,path) <- roots | startsWith path (modulePath m)] of
                [x:_] = Just x
                _     = Nothing

    sortByLabel nodes = map sortChildren (sortBy ordering nodes)
    where
        ordering a b = a.ChoiceTree.label < b.ChoiceTree.label

        sortChildren node=:{ChoiceTree|type=ExpandedNode children} = {node & type = ExpandedNode (sortByLabel children)}
        sortChildren node=:{ChoiceTree|type=CollapsedNode children} = {node & type = CollapsedNode (sortByLabel children)}
        sortChildren node = node

lookupModule :: ModuleName CodeBase -> Maybe (ModuleName,ModuleType,FilePath)
lookupModule module [] = Nothing
lookupModule module [t=:{SourceTree|modules}:ts]
    = maybe (lookupModule module ts) Just (find ((==) module o fst3) modules)

listFilesInCodeBase :: CodeBase -> [CleanFile]
listFilesInCodeBase codeBase
    = flatten [	[(rootPath, modName, Icl) \\ (modName,_,_)         <- modules] ++
    			[(rootPath, modName, Dcl) \\ (modName,AuxModule,_) <- modules]
	    	  \\ {SourceTree|rootPath,modules} <- codeBase]

    //TODO Also add dcl files

cleanFilePath :: CleanFile -> FilePath
cleanFilePath (baseDir,modName,ext) = foldl (</>) baseDir (split "." modName) +++ toString ext

getModuleType :: ModuleName CodeBase -> Maybe ModuleType
getModuleType modName [] = Nothing
getModuleType modName [{SourceTree|modules}:ts] = maybe (getModuleType modName ts) Just (search modules)
where
    search [] = Nothing
    search [(m,t,p):ms]
        | modName == m  = Just t
                        = search ms

codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]
codeBaseToCleanModuleNames codeBase
    = flatten [[(foldl (</>) rootPath (split "." modName), modName) \\ (modName,modType,modPath) <- modules] \\ {SourceTree|rootPath,modules} <- codeBase]

dirsOfTree :: !SourceTree -> [FilePath]
dirsOfTree {SourceTree|rootPath,subPaths=[]} = [rootPath]
dirsOfTree {SourceTree|rootPath,subPaths}    = [rootPath </> subPath \\ subPath <- subPaths]

findModulesForTree :: !SourceTree !*World -> ([(ModuleName,ModuleType,FilePath)],*World)
findModulesForTree tree w
    # (files,w) = foldr addDir ([],w) (dirsOfTree tree)
    = find [] files w
where
    addDir dir (files,w)
        # (filesInDir,w) = getFilesInPath dir w
        = ([dir </> file \\ file <- filesInDir] ++ files,w)

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
            = (addModule (dropExtension f) (toModuleName fileName modBase) False fsModules, w)
        | ext == "dcl"
            = (addModule (dropExtension f) (toModuleName fileName modBase) True fsModules, w)
        = (fsModules,w)

    addModule path modName isAux []
        = [(modName,if isAux AuxModule MainModule,path)]
    addModule path modName isAux [(m,MainModule,p):ms]
        | modName == m && isAux = [(m,AuxModule,p):ms]
                                = [(m,MainModule,p):addModule path modName isAux ms]
    addModule path modName isAux [(m,type,p):ms]
        | modName == m          = [(m,type,p):ms]
                                = [(m,type,p):addModule path modName isAux ms]


    toModuleName fileName modBase =join "." (reverse [fileName:modBase])

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

/*
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
*/
updateCleanEditor :: (Shared CodeMirror) -> Task CodeMirror
updateCleanEditor mirror
	= updateSharedInformation () [UpdateWith  (\cm -> codeMirrorEditlet cm [])
													  (\_ (Editlet value _ _) -> value)
										 ] mirror
    <<@ ForceLayout <<@ AfterLayout (tweakUI fill)

viewCleanEditor :: (Shared CodeMirror) -> Task CodeMirror
viewCleanEditor mirror
    = viewSharedInformation () [ViewWith (\cm -> codeMirrorEditlet cm [])] mirror

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


