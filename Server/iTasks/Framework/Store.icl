implementation module iTasks.Framework.Store

import StdEnv
import Data.Void
import Data.Maybe, Data.Map, Data.Functor, Data.Error
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

instance toString StoreReadError
where
    toString StoreReadMissingError         = "Store not found"
    toString StoreReadDataError            = "Failed to read store data"
    toString StoreReadTypeError            = "Stored data is is of incorrect type"
    toString StoreReadBuildVersionError    = "Stored data contains functions from an older executable that can no longer be evaluated"

safeName :: !String -> String
safeName s = copy 0 (createArray len '\0')
where
	len = size s
	copy :: !Int !*String -> String
	copy i n
		| i == len	= n
		| isAlphanum s.[i] || s.[i] == '-'  = copy (i + 1) {n & [i] = s.[i]}
							                = copy (i + 1) {n & [i] = '_'} 
singleValueStoreRead :: !StoreNamespace !StoreName !Bool !*IWorld -> (!MaybeError StoreReadError a,!*IWorld) | JSONDecode{|*|}, TC a
singleValueStoreRead namespace key checkBuildVersion iworld=:{IWorld|onClient=True}
    # (mbVal,iworld) = jsLoadValue namespace key iworld
	= (maybe (Error StoreReadMissingError) Ok mbVal, iworld)
singleValueStoreRead namespace key checkBuildVersion iworld=:{IWorld|server={buildID,paths={dataDirectory}}}
	# (mbItem,iworld) = readFromDisk namespace key iworld
	= case mbItem of
		Ok item
            # json = fromString item
            | checkBuildVersion
                = case json of
                    JSONArray [JSONString storedBuildID,jsonv]
                        | (storedBuildID == buildID) || (functionFree jsonv)
                            = (maybe (Error StoreReadTypeError) Ok (fromJSON jsonv), iworld)
                        | otherwise
                            = (Error StoreReadBuildVersionError,iworld)
                    _   = (Error StoreReadTypeError,iworld)
            | otherwise
                = (maybe (Error StoreReadTypeError) Ok (fromJSON json), iworld)
		Error e
            = (Error e,iworld)

singleValueStoreWrite :: !StoreNamespace !StoreName !Bool !a !*IWorld -> *IWorld | JSONEncode{|*|}, TC a
singleValueStoreWrite namespace key storeBuildVersion value iworld=:{IWorld|onClient=True}
	= jsStoreValue namespace key value iworld
singleValueStoreWrite namespace key storeBuildVersion value iworld=:{IWorld|server={buildID}}
	= writeToDisk namespace key (toString (if storeBuildVersion (toJSON (buildID,value)) (toJSON value))) iworld

singleValueStoreSDS :: !StoreNamespace !StoreName !Bool !Bool !(Maybe a) -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
singleValueStoreSDS namespace storeId checkBuild resetOnError defaultV = createReadWriteSDS namespace storeId read write
where
	read Void iworld
		# (mbV,iworld) = singleValueStoreRead namespace storeId checkBuild iworld
        = case (mbV,defaultV) of
            (Ok v,_)    = (Ok v, iworld)
            (Error StoreReadMissingError,Just def)
                # iworld = singleValueStoreWrite namespace storeId checkBuild def iworld
                = (Ok def,iworld)
            (Error e,Just def) | resetOnError
                # iworld = singleValueStoreWrite namespace storeId checkBuild def iworld
                = (Ok def,iworld)
            (Error e,Nothing) | resetOnError
                # iworld = deleteValue namespace storeId iworld
                = (Error (dynamic e,toString e), iworld)
            (Error e,_)
                = (Error (dynamic e,toString e), iworld)
	write Void v iworld
		= (Ok (const True),singleValueStoreWrite namespace storeId checkBuild v iworld)

blobStoreWrite :: !StoreNamespace !StoreName !{#Char} !*IWorld -> *IWorld
blobStoreWrite namespace key blob iworld=:{IWorld|onClient=True}
	= jsStoreValue namespace key blob iworld
blobStoreWrite namespace key blob iworld
	= writeToDisk namespace key blob iworld

blobStoreRead :: !StoreNamespace !StoreName !*IWorld -> (!MaybeError StoreReadError {#Char}, !*IWorld)
blobStoreRead namespace key iworld=:{onClient=True}
	# (mbBlob,iworld) =jsLoadValue namespace key iworld
    = (maybe (Error StoreReadMissingError) Ok mbBlob, iworld)
blobStoreRead namespace key iworld
    = readFromDisk namespace key iworld
	
writeToDisk :: !StoreNamespace !StoreName !{#Char} !*IWorld -> *IWorld
writeToDisk namespace key content iworld=:{server={buildID,paths={dataDirectory}},world}
	# location = dataDirectory </> "stores"
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
	
readFromDisk :: !StoreNamespace !StoreName !*IWorld -> (MaybeError StoreReadError {#Char}, !*IWorld)	
readFromDisk namespace key iworld=:{server={buildID,paths={dataDirectory}},world}
	# filename			= addExtension (dataDirectory </> "stores" </> namespace </> safeName key) "txt"
	# (ok,file,world)	= fopen filename FReadData world
	| ok
	    # (content,file)	= freadfile file
		# (ok,world)		= fclose file world
        | ok
		    = (Ok content, {iworld & world = world})
        | otherwise
            = (Error StoreReadDataError,{iworld & world = world})
    | otherwise
        = (Error StoreReadMissingError, {iworld & world = world})
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
		# storeDir		= dataDirectory </>"stores"</> namespace
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

listStoreNamespaces :: !*IWorld -> (![StoreNamespace], !*IWorld)
listStoreNamespaces iworld=:{server={buildID,paths={dataDirectory}},world}
    # (res,world)   = readDirectory (dataDirectory</>"stores") world
    = case res of
        Error e     = ([], {iworld & world = world})
        Ok files    = ([f \\ f <- files | not (f == "." || f == "..")], {iworld & world = world})

listStoreNames :: !StoreNamespace !*IWorld -> (!MaybeErrorString [StoreName], !*IWorld)
listStoreNames namespace iworld
    # (namespaces,iworld=:{server={buildID,paths={dataDirectory}},world})
                    = listStoreNamespaces iworld
    | not (isMember namespace namespaces)
                    = (Error ("Namespace " +++ namespace +++ " does not exist"), {iworld & world = world})
    # storeDir		= dataDirectory </>"stores"</> namespace
    # (res,world)   = readDirectory storeDir world
    = case res of
        Error e     = (Error (snd e), {iworld & world = world})
        Ok keys     = (Ok [dropExtension k \\ k <- keys | not (k == "." || k == "..")], {iworld & world = world})

