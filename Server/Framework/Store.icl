implementation module Store

import StdString, StdArray, StdChar, StdClass, StdInt, StdFile, StdList, StdMisc
import Directory, Maybe, Map, Text, JSON, Functor
from Time import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp
from Types import :: IWorld{store,world,timestamp}, :: Config
import dynamic_string //Static dynamic serialization

:: *Store =
	{ cache		:: *Map String (Bool,StoreItem)	//Cache for storage items, Bool is used to indicate a value in the cache is 'dirty'
	, location	:: String						//Path to the store on disk
	}
	
:: StoreItem =
	{ format	:: StoreFormat
	, content	:: String
	, timestamp	:: Timestamp
	}

:: StoreFormat = SFPlain | SFDynamic | SFBlob

/**
* Create a store
*/
createStore	:: !String -> *Store
createStore location = {Store|cache = newMap, location = location}

storeValue :: !String !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValue key value iworld = storeValueAs SFPlain key value iworld

storeValueAsBlob :: !String !String !*IWorld -> *IWorld
storeValueAsBlob key value iworld=:{IWorld|timestamp}
	= appCache (put key (True,{StoreItem|format=SFBlob,content=value,timestamp=timestamp})) iworld

storeValueAs :: !StoreFormat !String !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValueAs format key value iworld=:{IWorld|timestamp}
	= appCache (put key (True,{StoreItem|format=format,content=content,timestamp=timestamp})) iworld
where
	content = case format of	
		SFPlain		= toString (toJSON value)
		SFDynamic	= dynamic_to_string (dynamic value)

loadDynamicValue :: !String !*IWorld -> (!Maybe Dynamic,!*IWorld)
loadDynamicValue key iworld=:{store=store=:{location}}
	# (mbItem,iworld) = accCache (getU key) iworld
	= case mbItem of
		Just (dirty,item)
			= (unpackItem item,iworld)
		Nothing
			# (mbItem,iworld) = accWorld (loadFromDisk key location) iworld
			= case mbItem of
				Just item
					# iworld = appCache (put key (False,item)) iworld
					= (unpackItem item,iworld)
				Nothing
					= (Nothing,iworld)
where
	unpackItem {StoreItem | format=SFPlain, content} = Nothing
	unpackItem {StoreItem | format=SFDynamic, content} = Just (string_to_dynamic { s \\ s <-: content})

loadValueAsBlob :: !String !*IWorld -> (!Maybe String,!*IWorld)
loadValueAsBlob key iworld=:{store=store=:{location}}
	# (mbItem,iworld) = accCache (getU key) iworld
	= case mbItem of
		Just (dirty,item) = (unpackValue item,iworld)
		Nothing
			# (mbItem,iworld) = accWorld (loadFromDisk key location) iworld
			= case mbItem of
				Just item
					# iworld = appCache (put key (False,item)) iworld
					= (unpackValue item,iworld)
				Nothing
					= (Nothing,iworld)
where
	unpackValue {StoreItem|content} = Just content

loadValue :: !String !*IWorld -> (!Maybe a,!*IWorld) | JSONDecode{|*|}, TC a
loadValue key iworld
	# (mbItem,iworld) = loadStoreItem key iworld
	= case mbItem of
		Just item = case unpackValue item of
			Just v	= (Just v,iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)
		
getStoreTimestamp :: !String !*IWorld -> (!Maybe Timestamp,!*IWorld)
getStoreTimestamp key iworld
	# (mbItem,iworld) = loadStoreItem key iworld
	= case mbItem of
		Just item	= (Just item.StoreItem.timestamp,iworld)
		Nothing 	= (Nothing,iworld)

loadValueAndTimestamp :: !String !*IWorld -> (!Maybe (a,Timestamp),!*IWorld) | JSONDecode{|*|}, TC a
loadValueAndTimestamp key iworld
	# (mbItem,iworld) = loadStoreItem key iworld
	= case mbItem of
		Just item = case unpackValue item of
			Just v	= (Just (v,item.StoreItem.timestamp),iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)

unpackValue :: !StoreItem -> (Maybe a) | JSONDecode{|*|}, TC a
unpackValue {StoreItem|format=SFPlain,content}
	= case fromJSON (fromString content) of
		Nothing		= Nothing
		Just v		= Just v
unpackValue {StoreItem|format=SFBlob,content}
	= abort "use loadValueAsBlob"
unpackValue {StoreItem|format=SFDynamic,content,timestamp}
	= case string_to_dynamic {s` \\ s` <-: content} of
		(value :: a^)	= Just value
		_				= Nothing	
			
loadStoreItem :: !String !*IWorld -> (!Maybe StoreItem,!*IWorld)
loadStoreItem key iworld=:{store=store=:{location}}
	# (mbItem,iworld) = accCache (getU key) iworld
	= case mbItem of
		Just (dirty,item)
			# iworld = appCache (put key (dirty,item)) iworld
			= (Just item,iworld)
		Nothing
			# (mbItem,iworld) = accWorld (loadFromDisk key location) iworld
			= case mbItem of
				Just item
					# iworld = appCache (put key (False,item)) iworld
					= (Just item,iworld)
				Nothing
					= (Nothing,iworld)
				
loadFromDisk :: !String !String !*World -> (Maybe StoreItem, !*World)	
loadFromDisk key location world			
		//Try plain format first
		# filename			= location +++ "/" +++ key +++ ".txt"
		# (ok,file,world)	= fopen filename FReadText world
		| ok
			# (ok,time,file)	= freadi file
			# (ok,_,file)		= freadc file // read char indicating the end of the timestamp
			# (content,file)	= freadfile file
			# (ok,world)		= fclose file world
			= (Just {StoreItem|format = SFPlain, content = content, timestamp = Timestamp time}, world)
		| otherwise
			# filename			= location +++ "/" +++ key +++ ".bin"
			# (ok,file,world)	= fopen filename FReadData world
			| ok
				# (ok,time,file)	= freadi file
				# (content,file)	= freadfile file
				#( ok,world)		= fclose file world
				=(Just {StoreItem|format = SFDynamic, content = content, timestamp = Timestamp time}, world)
			| otherwise
				# filename 			= location +++ "/" +++ key +++ ".blb"
				# (ok,file,world)	= fopen filename FReadData world
				| ok
					# (ok,time,file)	= freadi file
					# (content,file)	= freadfile file
					# (ok,world)		= fclose file world
					=(Just {StoreItem|format = SFBlob, content = content, timestamp = Timestamp time}, world)				
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

deleteValue :: !String !*IWorld -> *IWorld
deleteValue delKey iworld = deleteValues` delKey (==) filterFuncDisk iworld
where
	// compare key with filename without extension
	filterFuncDisk delKey key = (subString 0 (size key - 4) key) == delKey

deleteValues :: !String !*IWorld -> *IWorld
deleteValues delKey iworld = deleteValues` delKey startsWith startsWith iworld

deleteValues` :: !String !(String String -> Bool) !(String String -> Bool) !*IWorld -> *IWorld
deleteValues` delKey filterFuncCache filterFuncDisk iworld=:{store=store=:{location}}
	//Delete items from cache
	# iworld = appCache (\cache -> fromList [(key,item) \\ (key,item) <- toList cache | not (filterFuncCache delKey key)]) iworld
	//Delete items from disk
	# iworld = appWorld deleteFromDisk iworld
	= iworld
where
	deleteFromDisk world
		# ((ok,dir),world)			= pd_StringToPath location world
		| not ok					= abort ("Cannot create path to " +++ location)
			# ((err,files),world)	= getDirectoryContents dir world
			| err <> NoDirError		= abort ("Cannot read store directory " +++ location)
			= unlink dir files world
		where
			unlink _ [] world
				= world
			unlink dir [f:fs] world
				| filterFuncDisk delKey f.fileName
					# (err,world) = fremove (pathDown dir f.fileName) world 
					= unlink dir fs world
				| otherwise
					= unlink dir fs world

			pathDown (RelativePath steps) step = RelativePath (steps ++ [PathDown step]) 
			pathDown (AbsolutePath dn steps) step = AbsolutePath dn (steps ++ [PathDown step])

copyValues :: !String !String !*IWorld -> *IWorld
copyValues fromprefix toprefix iworld=:{store=store=:{location}}
	//Copy items in the cache
	# iworld = appCache (\cache -> fromList (flatten [[(key,(dirty,item)): if (startsWith fromprefix key) [(newKey key, (True,item) )] [] ]\\ (key,(dirty,item)) <- toList cache])) iworld
	//Copy items on disk
	# iworld = appWorld (copyOnDisk fromprefix toprefix location) iworld
	= iworld
where
	newKey key	= toprefix +++ (key % (size fromprefix, size key))

	copyOnDisk fromprefix toprefix location world
		# ((ok,dir),world)		= pd_StringToPath location world
		| not ok				= abort ("Cannot create path to " +++ location)
 		# ((err,files),world)	= getDirectoryContents dir world
 		| err <> NoDirError		= abort ("Cannot read store directory " +++ location)
 		= copy fromprefix toprefix files world

	copy fromprefix toprefix [] world = world
	copy fromprefix toprefix [f:fs] world
		| startsWith fromprefix f.fileName
			# sfile	= location +++ "/" +++ f.fileName
			# dfile = location +++ "/" +++ toprefix +++ (f.fileName % (size fromprefix, size f.fileName))
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

flushCache :: !*IWorld -> *IWorld
flushCache iworld=:{world,store=store=:{cache,location}}
	//Check if the location exists and create it otherwise
	# ((ok,dir),world)	= pd_StringToPath location world
	| not ok			= abort ("Cannot create storepath: " +++ location)
	#(err,world)		= case getFileInfo dir world of
							((DoesntExist,fileinfo),world)	= createDirectory dir world
							(_,world)						= (NoDirError,world)
	# ok				= case err of NoDirError = True; _ = False
	| not ok			= abort ("Cannot create store: " +++ location)
	//Write the states to disk
	# (list, world) = flush (toList cache) world 
	= {iworld & world = world, store = {store & cache = fromList list}}
where
	flush [] world = ([],world)
	flush [(key,(False,item)):is] world
		# (is, world) = flush is world
		= ([(key,(False,item)):is], world)
	flush [(key,(True,item)):is] world
		# world = writeToDisk key item location world
		# (is, world) = flush is world
		= ([(key,(False,item)):is], world)

	writeToDisk key {StoreItem|format,content,timestamp} location world
		# filename 			= location +++ "/" +++ key +++ (case format of SFPlain = ".txt" ; SFDynamic = ".bin" ; SFBlob = ".blb")
		# (ok,file,world)	= fopen filename (case format of SFPlain = FWriteText; _ = FWriteData) world
		| not ok			= abort ("Failed to write value to store: " +++ filename)
		# file				= fwritei (toInt timestamp) file
		# file				= case format of SFPlain = fwritec ' ' file; _ = file // for txt files write space to indicate end of timestamp
		# file				= fwrites content file
		# (ok,world)		= fclose file world
		= world

isValueChanged :: !String !Timestamp !*IWorld -> (!Maybe Bool,!*IWorld)
isValueChanged key ts0 iworld
	# (mbTimestamp,iworld) = getStoreTimestamp key iworld
	= (fmap ((<) ts0) mbTimestamp,iworld)

appCache :: !.(*(Map String (Bool,StoreItem)) -> *(Map String (Bool,StoreItem))) !*IWorld -> *IWorld
appCache f iworld=:{store=store=:{cache}}
	= {iworld & store = {store & cache = f cache}}
	
accCache :: !.(*(Map String (Bool,StoreItem)) -> *(.a,*Map String (Bool,StoreItem))) !*IWorld -> (.a,*IWorld)
accCache f iworld=:{store=store=:{cache}}
	# (a,cache) = f cache
	= (a,{iworld & store = {store & cache = cache}})

appWorld :: !.(*World -> *World) !*IWorld -> *IWorld
appWorld f iworld=:{world}
	= {iworld & world = f world}
	
accWorld :: !.(*World -> *(.a,*World)) !*IWorld -> (.a,!*IWorld)
accWorld f iworld=:{world}
	# (a,world) = f world
	= (a,{iworld & world = world})