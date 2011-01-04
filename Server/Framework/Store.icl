implementation module Store

import StdString,StdMaybe, StdArray, StdChar, StdClass, StdInt, StdFile, StdList, StdMisc
import Directory, Time

import Map, Text
import JSON

import dynamic_string //Static dynamic serialization

derive JSONDecode Timestamp
derive bimap (,), Maybe

:: *Store =
	{ cache		:: *Map String (Bool,StoreItem)	//Cache for storage items, Bool is used to indicate a value in the cache is 'dirty'
	, location	:: String						//Path to the store on disk
	}
	
:: StoreItem =
	{ format	:: StoreFormat
	, content	:: String
	, timestamp	:: Maybe Timestamp // the timestamp is determined when the item is load first or written to disc
	}

:: StoreFormat = SFPlain | SFDynamic | SFBlob

/**
* Create a store
*/
createStore	:: !String -> *Store
createStore location = {Store|cache = newMap, location = location}

storeValue :: !String !a !*Store -> *Store | JSONEncode{|*|}, TC a
storeValue key value store = storeValueAs SFPlain key value store

storeValueAsBlob :: !String !String !*Store -> *Store
storeValueAsBlob key value store=:{cache}
	= {Store|store & cache = put key (True,{StoreItem|format=SFBlob,content=value,timestamp=Nothing}) cache}

storeValueAs :: !StoreFormat !String !a !*Store	-> *Store | JSONEncode{|*|}, TC a
storeValueAs format key value store=:{cache}
	= {Store|store & cache = put key (True,{StoreItem|format=format,content=content,timestamp=Nothing}) cache}
where
	content = case format of	
		SFPlain		= toString (toJSON value)
		SFDynamic	= dynamic_to_string (dynamic value)

loadDynamicValue :: !String !*Store !*World -> (!Maybe Dynamic, !*Store, !*World)
loadDynamicValue key store=:{cache,location} world
	#(mbItem,cache) = getU key cache
	= case mbItem of
		Just (dirty,item)
			= (unpackItem item, {store & cache = cache}, world)
		Nothing
			# (mbItem, world)	= loadFromDisk key location world
			= case mbItem of
				Just item
					# cache	= put key (False,item) cache
					= (unpackItem item, {store & cache = cache}, world)
				Nothing
					= (Nothing, {store & cache = cache}, world)
where
	unpackItem {StoreItem | format=SFPlain, content} = Nothing
	unpackItem {StoreItem | format=SFDynamic, content} = Just (string_to_dynamic { s \\ s <-: content})

loadValueAsBlob :: !String !*Store !*World -> (!Maybe String, !*Store, !*World)
loadValueAsBlob key store=:{cache,location} world
	#(mbItem,cache) = getU key cache
	= case mbItem of
		Just (dirty,item )= (unpackValue item, {store & cache = cache}, world)
		Nothing
			# (mbItem, world) = loadFromDisk key location world
			= case mbItem of
				Just item
					# cache	= put key (False,item) cache
					= (unpackValue item, {store & cache = cache}, world)
				Nothing
					= (Nothing, {store & cache = cache}, world)
where
	unpackValue {StoreItem|content} = Just content

loadValue :: !String !*Store !*World -> (!Maybe a, !*Store, !*World) | JSONDecode{|*|}, TC a
loadValue key store world
	# (mbValueAndTimestamp, store, world) = loadValueAndTimestamp key store world
	= case mbValueAndTimestamp of
		Nothing		= (Nothing, store, world)
		Just (v,_)	= (Just v, store, world)
	 
loadValueAndTimestamp :: !String !*Store !*World -> (!Maybe (a,Timestamp), !*Store, !*World) | JSONDecode{|*|}, TC a
loadValueAndTimestamp key store=:{cache,location} world
	#(mbItem,cache) = getU key cache
	= case mbItem of
		Just (dirty,item)
			# (item,cache,world) = case item.timestamp of
				Nothing // no timestamp determined yet
					# (t,world)	= time world
					# item		= {item & timestamp = Just t}
					# cache		= put key (dirty, item) cache
					= (item, cache, world)
				_ // timestamp already determined
					= (item, cache, world)
			= (unpackValue item, {store & cache = cache}, world)
		Nothing
			# (mbItem, world) = loadFromDisk key location world
			= case mbItem of
				Just item
					# cache	= put key (False,item) cache
					= (unpackValue item, {store & cache = cache}, world)
				Nothing
					= (Nothing, {store & cache = cache}, world)
where
	unpackValue {StoreItem|format=SFPlain,content,timestamp}
		= case fromJSON (fromString content) of
			Nothing		= Nothing
			Just v		= Just (v, fromJust timestamp)
	unpackValue {StoreItem|format=SFBlob,content}  = Nothing //<- use loadValueAsBlob
	unpackValue {StoreItem|format=SFDynamic,content,timestamp}
		= case string_to_dynamic {s` \\ s` <-: content} of
			(value :: a^)	= Just (value, fromJust timestamp)
			_				= Nothing
				
loadFromDisk :: String String !*World -> (Maybe StoreItem, !*World)	
loadFromDisk key location world			
		//Try plain format first
		# filename			= location +++ "/" +++ key +++ ".txt"
		# (ok,file,world)	= fopen filename FReadData world
		| ok
			# (ok,time,file)	= freadi file
			# (content,file)	= freadfile file
			# (ok,world)		= fclose file world
			= (Just {StoreItem|format = SFPlain, content = content, timestamp = Just (Timestamp time)}, world)
		| otherwise
			# filename			= location +++ "/" +++ key +++ ".bin"
			# (ok,file,world)	= fopen filename FReadData world
			| ok
				# (ok,time,file)	= freadi file
				# (content,file)	= freadfile file
				#( ok,world)		= fclose file world
				=(Just {StoreItem|format = SFDynamic, content = content, timestamp = Just (Timestamp time)}, world)
			| otherwise
				# filename 			= location +++ "/" +++ key +++ ".blb"
				# (ok,file,world)	= fopen filename FReadData world
				| ok
					# (ok,time,file)	= freadi file
					# (content,file)	= freadfile file
					# (ok,world)		= fclose file world
					=(Just {StoreItem|format = SFBlob, content = content, timestamp = Just (Timestamp time)}, world)				
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

deleteValues :: !String !*Store !*World -> (!*Store, !*World)
deleteValues prefix store=:{cache,location} world
	//Delete items from cache
	# cache = fromList [(key,item) \\ (key,item) <- toList cache | not (startsWith prefix key)]
	//Delete items from disk
	# world = deleteFromDisk prefix location world
	= ({store & cache = cache},world)
where
	deleteFromDisk prefix location world
		# ((ok,dir),world)		= pd_StringToPath location world
		| not ok				= abort ("Cannot create path to " +++ location)
 		# ((err,files),world)	= getDirectoryContents dir world
 		| err <> NoDirError		= abort ("Cannot read store directory " +++ location)
 		= unlink prefix dir files world
 		
	unlink prefix dir [] world
		= world
	unlink prefix dir [f:fs] world
		| startsWith prefix f.fileName
			# (err,world) = fremove (pathDown dir f.fileName) world 
			= unlink prefix dir fs world
		| otherwise
			= unlink prefix dir fs world

	pathDown (RelativePath steps) step = RelativePath (steps ++ [PathDown step]) 
	pathDown (AbsolutePath dn steps) step = AbsolutePath dn (steps ++ [PathDown step])

copyValues :: !String !String !*Store !*World -> (!*Store, !*World)
copyValues fromprefix toprefix store=:{cache,location} world
	//Copy items in the cache
	# cache = fromList (flatten [[(key,(dirty,item)): if (startsWith fromprefix key) [(newKey key, (True,item) )] [] ]\\ (key,(dirty,item)) <- toList cache])
	//Copy items on disk
	# world	= copyOnDisk fromprefix toprefix location world
	= ({store & cache = cache},world)
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
			# sfile	= (location +++ "/" +++ f.fileName)
			# dfile = (location +++ "/" +++ toprefix +++ (f.fileName % (size fromprefix, size f.fileName)))
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

flushCache :: !*Store !*World -> (!*Store,!*World)
flushCache store=:{cache,location} world
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
	= ({store & cache = fromList list}, world)
where
	flush [] world = ([],world)
	flush [(key,(False,item)):is] world
		# (is, world) = flush is world
		= ([(key,(False,item)):is], world)
	flush [(key,(True,item)):is] world
		// determine timestamp if done yet
		# (item, world) = case item.timestamp of
			Nothing
				# (t, world) = time world
				= ({item & timestamp = Just t}, world)
			_ = (item, world)
		# world = writeToDisk key item location world
		# (is, world) = flush is world
		= ([(key,(False,item)):is], world)

	writeToDisk key {StoreItem|format,content,timestamp} location world
		# filename 			= location +++ "/" +++ key +++ (case format of SFPlain = ".txt" ; SFDynamic = ".bin" ; SFBlob = ".blb")
		# (ok,file,world)	= fopen filename FWriteData world
		| not ok			= abort ("Failed to write value to store: " +++ filename)
		# file				= fwritei ((\(Just (Timestamp t)) -> t) timestamp) file
		# file				= fwrites content file
		# (ok,world)		= fclose file world
		= world

isValueChanged :: !String !Timestamp !*Store !*World -> (!Bool, !*Store, !*World)
isValueChanged key t store=:{location,cache} world
	# (mbItem,cache) = getU key cache
	# store = {store & cache = cache}
	# (mbTimestamp, store, world) = case mbItem of
		Just (_,{timestamp}) = (timestamp, store, world)
		Nothing
			# (mbItem, world) = loadFromDisk key location world
			= case mbItem of
				Nothing				= (Nothing, store, world)
				Just {timestamp}	= (timestamp, store, world)
	= case mbTimestamp of
		Nothing		= (True, store, world)
		timestamp	= (t < (fromJust timestamp), store, world)
	 