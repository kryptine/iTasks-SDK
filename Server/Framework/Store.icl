implementation module Store

import StdString, StdArray, StdChar, StdClass, StdInt, StdFile, StdList, StdTuple, StdMisc, Void
import File, Directory, OSError, Maybe, Map, Text, JSON, Functor, FilePath
from IWorld			import :: IWorld(..), :: ProcessId, :: Control
from SystemTypes	import :: DateTime, :: User, :: Config
from Time 			import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp
from iTasks import serialize, deserialize, defaultStoreFormat

:: Store =
	{ location	:: !String							//Path to the store on disk
	}
	
:: StoreItem =
	{ format		:: !StoreFormat
	, content		:: !String
	, lastChange	:: !Timestamp
	}

:: StoreFormat = SFPlain | SFDynamic

storeValue :: !StoreNamespace !StoreKey !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValue namespace key value iworld 
	= storeValueAs defaultStoreFormat namespace key value iworld

storeValueAs :: !StoreFormat !StoreNamespace !StoreKey !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValueAs format namespace key value iworld=:{IWorld|timestamp,world,storeDirectory}
	# world = writeToDisk namespace key {StoreItem|format=format,content=content,lastChange=timestamp} storeDirectory world
	= {iworld & world = world}
where
	content = case format of	
		SFPlain		= toString (toJSON value)
		SFDynamic	= serialize value

writeToDisk :: !StoreNamespace !StoreKey !StoreItem !String !*World -> *World
writeToDisk namespace key {StoreItem|format,content,lastChange} location world
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
	= world

loadValue :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe a,!*IWorld) | JSONDecode{|*|}, TC a
loadValue namespace key iworld
	# (mbItem,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue item of
			Just v	= (Just v,iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)
		
getStoreTimestamp :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe Timestamp,!*IWorld)
getStoreTimestamp namespace key iworld
	# (mbItem,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item	= (Just item.StoreItem.lastChange,iworld)
		Nothing 	= (Nothing,iworld)

loadValueAndTimestamp :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe (a,Timestamp),!*IWorld) | JSONDecode{|*|}, TC a
loadValueAndTimestamp namespace key iworld
	# (mbItem,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue item of
			Just v	= (Just (v,item.StoreItem.lastChange),iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)

unpackValue :: !StoreItem -> (Maybe a) | JSONDecode{|*|}, TC a
unpackValue {StoreItem|format=SFPlain,content}
	= case fromJSON (fromString content) of
		Nothing		= Nothing
		Just v		= Just v
unpackValue {StoreItem|format=SFDynamic,content,lastChange}
	= case deserialize {s` \\ s` <-: content} of
		Ok value = Just value
		Error _  = Nothing

loadStoreItem :: !StoreNamespace !StoreKey !*IWorld -> (!Maybe StoreItem,!*IWorld)
loadStoreItem namespace key iworld=:{storeDirectory}
	= accWorld (loadFromDisk namespace key storeDirectory) iworld
		
loadFromDisk :: !StoreNamespace !StoreKey !String !*World -> (Maybe StoreItem, !*World)	
loadFromDisk namespace key location world			
		//Try plain format first
		# filename			= addExtension (location </> namespace </> key) "txt"
		# (ok,file,world)	= fopen filename FReadText world
		| ok
			# (ok,time,file)	= freadi file
			# (ok,_,file)		= freadc file // read char indicating the end of the timestamp
			# (content,file)	= freadfile file
			# (ok,world)		= fclose file world
			= (Just {StoreItem|format = SFPlain, content = content, lastChange = Timestamp time}, world)
		| otherwise
			# filename			= addExtension (location </> key) "bin"
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
deleteValues` delKey filterFuncCache filterFuncDisk iworld=:{storeDirectory}
	//Delete items from disk
	# iworld = appWorld deleteFromDisk iworld
	= iworld
where
	deleteFromDisk world
		# (res, world) = readDirectory storeDirectory world
		| isError res = abort ("Cannot read store directory " +++ storeDirectory +++ ": " +++ snd (fromError res))
		= unlink storeDirectory (fromOk res) world
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
copyValues namespace fromprefix toprefix iworld=:{storeDirectory}
	//Copy items on disk
	# iworld = appWorld (copyOnDisk fromprefix toprefix storeDirectory) iworld
	= iworld
where
	newKey key	= toprefix +++ (key % (size fromprefix, size key))

	copyOnDisk fromprefix toprefix location world
 		# (res,world)	= readDirectory location world
 		| isError res	= abort ("Cannot read store directory " +++ location +++ ": " +++ snd (fromError res))
 		= copy fromprefix toprefix (fromOk res) world

	copy fromprefix toprefix [] world = world
	copy fromprefix toprefix [f:fs] world
		| startsWith fromprefix f
			# sfile	= storeDirectory </> f
			# dfile = storeDirectory </> toprefix +++ (f % (size fromprefix, size f))
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