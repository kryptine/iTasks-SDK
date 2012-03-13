implementation module Store

import StdString, StdArray, StdChar, StdClass, StdInt, StdBool, StdFile, StdList, StdTuple, StdOrdList, StdMisc, Void
import File, Directory, OSError, Maybe, Map, Text, JSON, Functor, FilePath
from IWorld			import :: IWorld(..)
from SystemTypes	import :: DateTime, :: User, :: Config, :: TaskId, :: TaskNo, :: TopNo, :: TaskListItem, :: TaskTime 
from TaskState		import :: ParallelItem, :: ParallelControl
from Time 			import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp
from iTasks import serialize, deserialize, defaultStoreFormat, functionFree

:: StoreItem =
	{ format		:: !StoreFormat
	, content		:: !String
	, version		:: !Int
	}

:: StoreFormat = SFPlain | SFDynamic

storePath :: !FilePath !String -> FilePath
storePath dataDir build = dataDir </> "store-" +++ build

storeValue :: !StoreNamespace !StoreKey !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValue namespace key value iworld 
	= storeValueAs defaultStoreFormat namespace key value iworld

storeValueAs :: !StoreFormat !StoreNamespace !StoreKey !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValueAs format namespace key value iworld=:{IWorld|build,dataDirectory} 
	//# (version,iworld) = getStoreVersion namespace key iworld
	= writeToDisk namespace key {StoreItem|format=format,content=content,version=0} (storePath dataDirectory build) iworld
where
	content = case format of	
		SFPlain		= toString (toJSON value)
		SFDynamic	= serialize value

writeToDisk :: !StoreNamespace !StoreKey !StoreItem !String !*IWorld -> *IWorld
writeToDisk namespace key {StoreItem|format,content,version} location iworld=:{IWorld|world}
	//Check if the location exists and create it otherwise
	# (exists,world)	= fileExists location world
	# world				= if exists world
							( case createDirectory location world of
								(Ok Void, world) = world
								(Error e, world) = abort ("Cannot create store: " +++ location +++ ": " +++ snd e)
							)
	//Check if the namespace exists and create it otherwise
	# (exists,world)	= fileExists (location </> namespace) world
	# world				= if exists world
							( case createDirectory (location </> namespace) world of
								(Ok Void, world) = world
								(Error e, world) = abort ("Cannot create namespace " +++ namespace +++ ": " +++ snd e)
							)
	//Write the value
	# filename 			= addExtension (location </> namespace </> key) (case format of SFPlain = "txt" ; SFDynamic = "bin")
	# (ok,file,world)	= fopen filename (case format of SFPlain = FWriteText; _ = FWriteData) world
	| not ok			= abort ("Failed to write value to store: " +++ filename +++ "\n")
	//Increment the version at each write to disk
	# file				= fwritei (version + 1) file
	# file				= case format of SFPlain = fwritec ' ' file; _ = file // for txt files write space to indicate end of timestamp
	# file				= fwrites content file
	# (ok,world)		= fclose file world
	= {IWorld|iworld & world = world}
	
loadValue :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe a,!*IWorld) | JSONDecode{|*|}, TC a
loadValue namespace key iworld=:{IWorld|build,dataDirectory}
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue (not old) item of
			Just v	= (Just v, if old (writeToDisk namespace key item (storePath dataDirectory build) iworld) iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)
		
/*getStoreVersion :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe Int,!*IWorld)
getStoreVersion namespace key iworld
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item	= (Just item.StoreItem.version,iworld)
		Nothing 	= (Nothing,iworld)

loadValueAndVersion :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe (a,Int),!*IWorld) | JSONDecode{|*|}, TC a
loadValueAndVersion namespace key iworld=:{IWorld|build,dataDirectory}
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue (not old) item of
			Just v
				= (Just (v,item.StoreItem.version), if old (writeToDisk namespace key item (storePath dataDirectory build) iworld) iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)*/

unpackValue :: !Bool !StoreItem -> (Maybe a) | JSONDecode{|*|}, TC a
unpackValue allowFunctions {StoreItem|format=SFPlain,content}
	# json = fromString content
	| allowFunctions || functionFree json
		= case fromJSON (fromString content) of
			Nothing		= Nothing
			Just v		= Just v
	| otherwise
		= Nothing
unpackValue allowFunctions {StoreItem|format=SFDynamic,content,version}
	= case deserialize {s` \\ s` <-: content} of
		Ok value = Just value
		Error _  = Nothing

loadStoreItem :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe StoreItem,!Bool,!*IWorld)
loadStoreItem namespace key iworld=:{build,dataDirectory}
	= case accWorld (loadFromDisk namespace key (storePath dataDirectory build)) iworld of
		(Just item,iworld)	= (Just item,False,iworld)
		(Nothing,iworld)	
			| namespace == NS_APPLICATION_SHARES
				# (mbItem,iworld) = findOldStoreItem namespace key iworld
				= (mbItem,True,iworld)
			| otherwise
				= (Nothing,False,iworld)

//Look in stores of previous builds for a version of the store that can be migrated
findOldStoreItem :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe StoreItem,!*IWorld)
findOldStoreItem namespace key iworld=:{application,build,appDirectory,dataDirectory,world}
	# (builds,world) = readBuilds dataDirectory world
	  //Also Look in 'old' data directory
	# (deprBuilds,world) = readBuilds (appDirectory </> application) world
	#  builds = builds ++ deprBuilds
	# (mbItem,world) = searchStoreItem (sortBy (\x y -> x > y) builds)/*(filter (startsWith "store-") dirs))*/ world
	= (mbItem,{IWorld|iworld & world = world})
where
	readBuilds dir world		
		# (mbBuilds,world)	= readDirectory dir world
		= case mbBuilds of
			Ok dirs	= ([dir </> d \\ d <- dirs | startsWith "store-" d],world)
			Error _	= ([],world)
			
	searchStoreItem [] world = (Nothing,world)
	searchStoreItem [d:ds] world
		= case loadFromDisk namespace key d world of
			(Just item, world)		
				= (Just item,world)
			(Nothing, world)
				= searchStoreItem ds world

loadFromDisk :: !StoreNamespace !StoreKey !FilePath !*World -> (Maybe StoreItem, !*World)	
loadFromDisk namespace key storeDir world		
		//Try plain format first
		# filename			= addExtension (storeDir </> namespace </> key) "txt"
		# (ok,file,world)	= fopen filename FReadText world
		| ok
			# (ok,version,file)	= freadi file
			# (ok,_,file)		= freadc file // read char indicating the end of the timestamp
			# (content,file)	= freadfile file
			# (ok,world)		= fclose file world
			= (Just {StoreItem|format = SFPlain, content = content, version = version}, world)
		| otherwise
			# filename			= addExtension (storeDir </> namespace </> key) "bin"
			# (ok,file,world)	= fopen filename FReadData world
			| ok
				# (ok,version,file)	= freadi file
				# (content,file)	= freadfile file
				#( ok,world)		= fclose file world
				=(Just {StoreItem|format = SFDynamic, content = content, version = version}, world)
			| otherwise
				= (Nothing, world)
where
	freadfile file = rec file ""
	where 
		rec :: *File String -> (String, *File)
		rec file acc 
			# (string, file) = freads file 102400
			| string == "" = (acc, file)
			| otherwise    = rec file (acc +++ string)

deleteValue :: !StoreNamespace !StoreKey !*IWorld -> *IWorld
deleteValue namespace delKey iworld = deleteValues` delKey (==) filterFuncDisk iworld
where
	// compare key with filename without extension
	filterFuncDisk delKey key = (subString 0 (size key - 4) key) == delKey

deleteValues :: !StoreNamespace !StorePrefix !*IWorld -> *IWorld
deleteValues namespace delKey iworld = deleteValues` delKey startsWith startsWith iworld

deleteValues` :: !String !(String String -> Bool) !(String String -> Bool) !*IWorld -> *IWorld
deleteValues` delKey filterFuncCache filterFuncDisk iworld=:{build,dataDirectory}
	//Delete items from disk
	# iworld = appWorld deleteFromDisk iworld
	= iworld
where
	deleteFromDisk world
		# storeDir		= storePath dataDirectory build
		# (res, world)	= readDirectory storeDir world
		| isError res = abort ("Cannot read store directory " +++ storeDir +++ ": " +++ snd (fromError res))
		= unlink storeDir (fromOk res) world
		where
			unlink _ [] world
				= world
			unlink dir [f:fs] world
				| filterFuncDisk delKey f
					# (err,world) = deleteFile (dir </> f) world 
					= unlink dir fs world
				| otherwise
					= unlink dir fs world

/*isValueChanged :: !StoreNamespace !StoreKey !Int !*IWorld -> (!Maybe Bool,!*IWorld)
isValueChanged namespace key v0 iworld
	# (mbVersion,iworld) = getStoreVersion namespace key iworld
	= (fmap ((<) v0) mbVersion,iworld)*/

appWorld :: !.(*World -> *World) !*IWorld -> *IWorld
appWorld f iworld=:{world}
	= {iworld & world = f world}
	
accWorld :: !.(*World -> *(.a,*World)) !*IWorld -> (.a,!*IWorld)
accWorld f iworld=:{world}
	# (a,world) = f world
	= (a,{iworld & world = world})
