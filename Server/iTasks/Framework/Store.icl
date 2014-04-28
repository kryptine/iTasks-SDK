implementation module iTasks.Framework.Store

import StdEnv
import Data.Void
import Data.Maybe, Data.Map, Data.Functor
import System.File, System.Directory, System.OSError, System.FilePath
import Text, Text.JSON

import iTasks.Framework.Client.JSStore
import iTasks.Framework.SDS

from iTasks.Framework.IWorld		import :: IWorld {onClient,server,world}, :: ServerInfo(..), :: SystemPaths(..), :: Resource
from iTasks.Framework.UIDefinition	import :: UIDef, :: UIControl, :: UIEditletOpts
from iTasks.Framework.UIDiff		import :: UIUpdate, :: UIEditletDiffs
from iTasks.Framework.TaskState		import :: TaskListEntry
from iTasks.API.Core.Types	        import :: DateTime, :: User, :: Config, :: TaskId, :: TaskNo, :: InstanceNo, :: TaskListItem, :: TaskTime, :: SessionId
from iTasks							import serialize, deserialize, functionFree
from System.Time 					import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp

storeAccess :: !StoreNamespace !StoreName !(Maybe a) -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
storeAccess namespace storeId defaultV = createReadWriteSDS namespace storeId read write
where
	read Void iworld
		# (mbV,iworld) = loadValue namespace storeId iworld
		= (maybe (maybe (Error ("Can't read " +++ storeId)) Ok defaultV) Ok mbV, iworld)
	write Void v iworld
		= (Ok (const True),storeValue namespace storeId v iworld)

storePath :: !FilePath !String -> FilePath
storePath dataDir buildID = dataDir </> "store-" +++ buildID

safeName :: !String -> String
safeName s = copy 0 (createArray len '\0') 
where
	len = size s
	copy :: !Int !*String -> String
	copy i n
		| i == len	= n
		| isAlphanum s.[i] || s.[i] == '-'  = copy (i + 1) {n & [i] = s.[i]}
							                = copy (i + 1) {n & [i] = '_'} 

storeValue :: !StoreNamespace !StoreName !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValue namespace key value iworld 
	= storeValueAs namespace key value iworld

storeValueAs :: !StoreNamespace !StoreName !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
storeValueAs namespace key value iworld=:{IWorld|onClient=True}
	= jsStoreValue namespace key value iworld
storeValueAs namespace key value iworld=:{IWorld|server={buildID,paths={dataDirectory}}}
	= writeToDisk namespace key (toString (toJSON value)) (storePath dataDirectory buildID) iworld

storeBlob :: !StoreNamespace !StoreName !{#Char}		!*IWorld -> *IWorld
storeBlob namespace key blob iworld=:{IWorld|onClient=True}
	= jsStoreValue namespace key blob iworld
storeBlob namespace key blob iworld=:{IWorld|server={buildID,paths={dataDirectory}}}
	= writeToDisk namespace key blob (storePath dataDirectory buildID) iworld
	
writeToDisk :: !StoreNamespace !StoreName !{#Char} !FilePath !*IWorld -> *IWorld
writeToDisk namespace key content location iworld=:{IWorld|world}
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
	# filename 			= addExtension (location </> namespace </> safeName key) "txt"
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= abort ("Failed to write value to store: " +++ filename +++ "\n")
	# file				= fwrites content file
	# (ok,world)		= fclose file world
	= {IWorld|iworld & world = world}
	
loadValue :: !StoreNamespace !StoreName !*IWorld -> (!Maybe a,!*IWorld) | JSONDecode{|*|}, TC a
loadValue namespace key iworld=:{IWorld|onClient=True}
	= jsLoadValue namespace key iworld
loadValue namespace key iworld=:{IWorld|server={buildID,paths={dataDirectory}}}
	# (mbItem,old,iworld) = loadStoreItem namespace key iworld
	= case mbItem of
		Just item = case unpackValue (not old) item of
			Just v	= (Just v, if old (writeToDisk namespace key item (storePath dataDirectory buildID) iworld) iworld)
			Nothing	= (Nothing,iworld)
		Nothing 	= (Nothing,iworld)
		
unpackValue :: !Bool !{#Char} -> (Maybe a) | JSONDecode{|*|}, TC a
unpackValue allowFunctions content
	# json = fromString content
	| allowFunctions || functionFree json
		= case fromJSON (fromString content) of
			Nothing		= Nothing
			Just v		= Just v
	| otherwise
		= Nothing

loadStoreItem :: !StoreNamespace !StoreName !*IWorld -> (!Maybe {#Char},!Bool,!*IWorld)
loadStoreItem namespace key iworld=:{server={buildID,paths={dataDirectory}},world}
	= case loadFromDisk namespace key (storePath dataDirectory buildID) world of
		(Just item,world)	= (Just item,False,{iworld & world = world})
		(Nothing,world)	
			| namespace == NS_APPLICATION_SHARES
				# (mbItem,iworld) = findOldStoreItem namespace key {iworld & world = world}
				= (mbItem,True,iworld)
			| otherwise
				= (Nothing,False,{iworld & world = world})

loadBlob :: !StoreNamespace !StoreName !*IWorld -> (!Maybe {#Char}, !*IWorld)
loadBlob namespace key iworld=:{onClient=True}
	= jsLoadValue namespace key iworld
loadBlob namespace key iworld=:{server={buildID,paths={dataDirectory}},world}
    # (mbContent,world) = loadFromDisk namespace key (storePath dataDirectory buildID) world
	= (mbContent, {IWorld|iworld & world = world})
	
//Look in stores of previous builds for a version of the store that can be migrated
findOldStoreItem :: !StoreNamespace !StoreName !*IWorld -> (!Maybe {#Char},!*IWorld)
findOldStoreItem namespace key iworld=:{server={serverName,paths={appDirectory,dataDirectory}},world}
	# (builds,world) = readBuilds dataDirectory world
	  //Also Look in 'old' data directory
	# (deprBuilds,world) = readBuilds (appDirectory </> serverName) world
	#  builds = builds ++ deprBuilds
	# (mbItem,world) = searchStoreItem (sortBy (\x y -> x > y) builds) world
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

loadFromDisk :: !StoreNamespace !StoreName !FilePath !*World -> (Maybe {#Char}, !*World)	
loadFromDisk namespace key storeDir world		
		//Try plain format first
		# filename			= addExtension (storeDir </> namespace </> safeName key) "txt"
		# (ok,file,world)	= fopen filename FReadData world
		| ok
			# (content,file)	= freadfile file
			# (ok,world)		= fclose file world
			= (Just content, world)
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

deleteValue :: !StoreNamespace !StoreName !*IWorld -> *IWorld
deleteValue namespace delKey iworld=:{onClient=True}
	= jsDeleteValue namespace delKey iworld
deleteValue namespace delKey iworld = deleteValues` namespace delKey (==) filterFuncDisk iworld
where
	// compare key with filename without extension
	filterFuncDisk delKey key = dropExtension key == delKey

deleteValues :: !StoreNamespace !StorePrefix !*IWorld -> *IWorld
deleteValues namespace delKey iworld = deleteValues` namespace delKey startsWith startsWith iworld

deleteValues` :: !String !String !(String String -> Bool) !(String String -> Bool) !*IWorld -> *IWorld
deleteValues` namespace delKey filterFuncCache filterFuncDisk iworld=:{server={buildID,paths={dataDirectory}},world}
	//Delete items from disk
	# world = deleteFromDisk world
	= {iworld & world = world}
where
	deleteFromDisk world
		# storeDir		= storePath dataDirectory buildID </> namespace
		# (res, world)	= readDirectory storeDir world
		| isError res = abort ("Cannot read store directory " +++ storeDir +++ ": " +++ snd (fromError res))
		= unlink storeDir (fromOk res) world
		where
			unlink _ [] world
				= world
			unlink dir [f:fs] world
				| filterFuncDisk (safeName delKey) f
					# (err,world) = deleteFile (dir </> f) world 
					= unlink dir fs world
				| otherwise
					= unlink dir fs world

listStores :: !StoreNamespace !*IWorld -> (![StoreName], !*IWorld)
listStores namespace iworld=:{server={buildID,paths={dataDirectory}},world}
    # storeDir		= storePath dataDirectory buildID </> namespace
    # (res,world)   = readDirectory storeDir world
    = case res of
        Error e     = ([], {iworld & world = world})
        Ok keys     = ([dropExtension k \\ k <- keys | not (k == "." || k == "..")], {iworld & world = world})

