implementation module Store

import StdString, StdArray, StdChar, StdClass, StdInt, StdBool, StdFile, StdList, StdTuple, StdOrdList, StdMisc, Void
import File, Directory, OSError, Maybe, Map, Text, JSON, Functor, FilePath
from IWorld			import :: IWorld(..), :: ProcessId, :: Control, :: Shared, :: RWShared
from SystemTypes	import :: DateTime, :: User, :: Config
from Time 			import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp
from iTasks import serialize, deserialize, defaultStoreFormat, functionFree

:: Store =
	{ location	:: !String							//Path to the store on disk
	}
	
:: StoreItem =
	{ format		:: !StoreFormat
	, content		:: !String
	, lastChange	:: !Timestamp
	}

:: StoreFormat = SFPlain | SFDynamic

storePath :: FilePath String String -> FilePath
storePath appDir app build = appDir </> app </> "store-" +++ build

storeValue :: !StoreNamespace !StoreKey !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValue namespace key value iworld 
	= storeValueAs defaultStoreFormat namespace key value iworld

storeValueAs :: !StoreFormat !StoreNamespace !StoreKey !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValueAs format namespace key value iworld=:{IWorld|application,build,appDirectory,timestamp}
	= writeToDisk namespace key {StoreItem|format=format,content=content,lastChange=timestamp} (storePath appDirectory application build) iworld
where
	content = case format of	
		SFPlain		= toString (toJSON value)
		SFDynamic	= serialize value

writeToDisk :: !StoreNamespace !StoreKey !StoreItem !String !*IWorld -> *IWorld
writeToDisk namespace key {StoreItem|format,content,lastChange} location iworld=:{IWorld|world}
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
	| not ok			= abort ("Failed to write value to store: " +++ filename)
	# file				= fwritei (toInt lastChange) file
	# file				= case format of SFPlain = fwritec ' ' file; _ = file // for txt files write space to indicate end of timestamp
	# file				= fwrites content file
	# (ok,world)		= fclose file world
	= {IWorld|iworld & world = world}
	
loadValue :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe a,!*IWorld) | JSONDecode{|*|}, TC a
loadValue namespace key iworld=:{IWorld|application,build,appDirectory}
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue (not old) item of
			Just v	= (Just v, if old (writeToDisk namespace key item (storePath appDirectory application build) iworld) iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)
		
getStoreTimestamp :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe Timestamp,!*IWorld)
getStoreTimestamp namespace key iworld
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item	= (Just item.StoreItem.lastChange,iworld)
		Nothing 	= (Nothing,iworld)

loadValueAndTimestamp :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe (a,Timestamp),!*IWorld) | JSONDecode{|*|}, TC a
loadValueAndTimestamp namespace key iworld=:{IWorld|application,build,appDirectory}
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue (not old) item of
			Just v
				= (Just (v,item.StoreItem.lastChange), if old (writeToDisk namespace key item (storePath appDirectory application build) iworld) iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)

unpackValue :: !Bool !StoreItem -> (Maybe a) | JSONDecode{|*|}, TC a
unpackValue allowFunctions {StoreItem|format=SFPlain,content}
	# json = fromString content
	| allowFunctions || functionFree json
		= case fromJSON (fromString content) of
			Nothing		= Nothing
			Just v		= Just v
	| otherwise
		= Nothing
unpackValue allowFunctions {StoreItem|format=SFDynamic,content,lastChange}
	= case deserialize {s` \\ s` <-: content} of
		Ok value = Just value
		Error _  = Nothing

loadStoreItem :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe StoreItem,!Bool,!*IWorld)
loadStoreItem namespace key iworld=:{application,build,appDirectory}
	= case accWorld (loadFromDisk namespace key (storePath appDirectory application build)) iworld of
		(Just item,iworld)	= (Just item,False,iworld)
		(Nothing,iworld)	
			| namespace == NS_APPLICATION_SHARES
				# (mbItem,iworld) = findOldStoreItem namespace key iworld
				= (mbItem,True,iworld)
			| otherwise
				= (Nothing,False,iworld)

//Look in stores of previous builds for a version of the store that can be migrated
findOldStoreItem :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe StoreItem,!*IWorld)
findOldStoreItem namespace key iworld=:{application,build,appDirectory,world}
	# (mbBuilds,world) = readDirectory baseDir world
	= case mbBuilds of
		Error _	= (Nothing, {IWorld|iworld & world = world})
		Ok dirs	
			# (mbItem,world) = searchStoreItem (sortBy (\x y -> x > y) (filter (startsWith "store-") dirs)) world
			= (mbItem,{IWorld|iworld & world = world})
where
	baseDir = appDirectory </> application

	searchStoreItem [] world = (Nothing,world)
	searchStoreItem [d:ds] world
		= case loadFromDisk namespace key (baseDir </> d) world of
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
			# (ok,time,file)	= freadi file
			# (ok,_,file)		= freadc file // read char indicating the end of the timestamp
			# (content,file)	= freadfile file
			# (ok,world)		= fclose file world
			= (Just {StoreItem|format = SFPlain, content = content, lastChange = Timestamp time}, world)
		| otherwise
			# filename			= addExtension (storeDir </> namespace </> key) "bin"
			# (ok,file,world)	= fopen filename FReadData world
			| ok
				# (ok,time,file)	= freadi file
				# (content,file)	= freadfile file
				#( ok,world)		= fclose file world
				=(Just {StoreItem|format = SFDynamic, content = content, lastChange = Timestamp time}, world)
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
deleteValues` delKey filterFuncCache filterFuncDisk iworld=:{application,build,appDirectory}
	//Delete items from disk
	# iworld = appWorld deleteFromDisk iworld
	= iworld
where
	deleteFromDisk world
		# storeDir		= storePath appDirectory application build
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

copyValues :: !StoreNamespace !StorePrefix !StorePrefix !*IWorld -> *IWorld
copyValues namespace fromprefix toprefix iworld=:{application,build,appDirectory}
	//Copy items on disk
	# iworld = appWorld (copyOnDisk fromprefix toprefix storeDir) iworld
	= iworld
where
	storeDir = storePath appDirectory application build
	newKey key	= toprefix +++ (key % (size fromprefix, size key))

	copyOnDisk fromprefix toprefix location world
 		# (res,world)	= readDirectory location world
 		| isError res	= abort ("Cannot read store directory " +++ location +++ ": " +++ snd (fromError res))
 		= copy fromprefix toprefix (fromOk res) world

	copy fromprefix toprefix [] world = world
	copy fromprefix toprefix [f:fs] world
		| startsWith fromprefix f
			# sfile	= storeDir </> f
			# dfile = storeDir </> toprefix +++ (f % (size fromprefix, size f))
			# world	= fcopy sfile dfile world
			= copy fromprefix toprefix fs world
		| otherwise
			= copy fromprefix toprefix fs world

	fcopy sfilename dfilename world
		# (ok,sfile,world) = fopen sfilename FReadData world
		| not ok = abort ("fcopy: Could not open " +++ sfilename +++ " for reading")
		# (ok,dfile,world) = fopen dfilename FWriteData world
		| not ok = abort ("fcopy: Could not open " +++ dfilename +++ " for writing")
		# (sfile,dfile) = transfer sfile dfile
		# (ok,world) = fclose sfile world
		| not ok = abort ("fcopy: Could not close " +++ sfilename)
		# (ok,world) = fclose dfile world
		| not ok = abort ("fcopy: Could not close " +++ dfilename)
		= world
	where
		transfer sfile dfile
			# (ok,c,sfile) = freadc sfile
			| ok
				# dfile = fwritec c dfile
				= transfer sfile dfile
			| otherwise
				# (err,sfile)= ferror sfile
				| err		= abort "fcopy: read error during copy"
				| otherwise = (sfile,dfile)	

isValueChanged :: !StoreNamespace !StoreKey !Timestamp !*IWorld -> (!Maybe Bool,!*IWorld)
isValueChanged namespace key ts0 iworld
	# (mbTimestamp,iworld) = getStoreTimestamp namespace key iworld
	= (fmap ((<) ts0) mbTimestamp,iworld)

appWorld :: !.(*World -> *World) !*IWorld -> *IWorld
appWorld f iworld=:{world}
	= {iworld & world = f world}
	
accWorld :: !.(*World -> *(.a,*World)) !*IWorld -> (.a,!*IWorld)
accWorld f iworld=:{world}
	# (a,world) = f world
	= (a,{iworld & world = world})