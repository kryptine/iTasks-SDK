implementation module Store

import StdString,StdMaybe, StdArray, StdChar, StdClass, StdInt, StdFile, StdList, StdMisc
import Directory

import Map, Text
import GenPrint
import GenParse

import StdDebug

import dynamic_string //Static dynamic serialization

:: *Store =
	{ cache		:: *Map String (Bool,StoreItem)	//Cache for storage items, Bool is used to indicate a value in the cache is 'dirty'
	, location	:: String						//Path to the store on disk
	}
	
:: StoreItem =
	{ format	:: StoreFormat
	, content	:: String
	}

:: StoreFormat = SFPlain | SFDynamic

/**
* Create a store
*/
createStore	:: !String -> *Store
createStore location = {Store|cache = newMap, location = location}

storeValue :: !String !a !*Store -> *Store | gPrint{|*|}, TC a
storeValue key value store = storeValueAs SFPlain key value store

storeValueAs :: !StoreFormat !String !a !*Store	-> *Store | gPrint{|*|}, TC a
storeValueAs format key value store=:{cache}
	= {Store|store & cache = put key (True,{StoreItem|format=format,content=content}) cache}
where
	content = case format of	
		SFPlain		= printToString value
		SFDynamic	= dynamic_to_string (dynamic value)

loadValue :: !String !*Store !*World -> (!Maybe a, !*Store, !*World) | gParse{|*|}, TC a
loadValue key store=:{cache,location} world
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
	unpackValue {StoreItem|format=SFPlain,content} = parseString content
	unpackValue {StoreItem|format=SFDynamic,content}
		= case string_to_dynamic {s` \\ s` <-: content} of
			(value :: a^)	= Just value
			_				= Nothing
	
	loadFromDisk key location world			
		//Try plain format first
		# filename			= location +++ "/" +++ key +++ ".txt"
		# (ok,file,world)	= fopen filename FReadData world
		| ok
			# (content,file)= freadfile file
			# (ok,world)	= fclose file world
			= (Just {StoreItem|format = SFPlain, content = content}, world)
		| otherwise
			//Try dynamic format
			# filename			= location +++ "/" +++ key +++ ".bin"
			# (ok,file,world)	= fopen filename FReadData world
			| ok
				#(content,file)	= freadfile file
				#(ok,world)		= fclose file world
				=(Just {StoreItem|format = SFDynamic, content = content}, world)
			| otherwise
				= (Nothing, world)

	freadfile file = rec file ""
	  where rec :: *File String -> (String, *File)
	        rec file acc # (string, file) = freads file 100
	                     | string == "" = (acc, file)
	                     | otherwise    = rec file (acc +++ string)



deleteValues :: !String !*Store !*World -> (!*Store, !*World)
deleteValues prefix store=:{cache,location} world
	//Delete items from cache
	# cache = trace_n("Delete values for " +++ prefix) fromList [(key,item) \\ (key,item) <- toList cache | not (startsWith prefix key)]
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
			# (err,world) = trace_n("Removing file: "+++f.fileName) fremove (pathDown dir f.fileName) world 
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
		# world = writeToDisk key item location world
		# (is, world) = flush is world
		= ([(key,(False,item)):is], world)

	writeToDisk key {StoreItem|format,content} location world
		# filename 			= location +++ "/" +++ key +++ (case format of SFPlain = ".txt" ; SFDynamic = ".bin")
		# (ok,file,world)	= fopen filename FWriteData world
		| not ok			= abort ("Failed to write value to store: " +++ filename)
		# file				= trace_n("Writing file to disk: "+++filename) fwrites content file
		# (ok,world)		= fclose file world
		= world

