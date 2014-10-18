implementation module iTasks.Framework.Store

import StdEnv
import Data.Void
import Data.Maybe, Data.Map, Data.Functor, Data.Error
import System.File, System.Directory, System.OSError, System.FilePath
import Text, Text.JSON

import iTasks.Framework.Client.JSStore
import iTasks.Framework.SDS

from iTasks.Framework.IWorld		import :: IWorld {onClient,server,memoryShares,cachedShares,world}, :: ServerInfo(..), :: SystemPaths(..), :: Resource, :: ShareCache(..)
from iTasks.Framework.UIDefinition	import :: UIDef, :: UIControl, :: UIEditletOpts
from iTasks.Framework.UIDiff		import :: UIUpdate, :: UIEditletDiffs
from iTasks.Framework.Task		    import exception
from iTasks.Framework.TaskState		import :: DeferredJSON(..)
from iTasks.Framework.Generic				import class iTask
from iTasks.Framework.Generic.Interaction	import generic gEditor, generic gEditMeta, generic gVerify, generic gUpdate, :: VSt, ::USt, :: VisualizationResult,:: EditMeta, :: VerifyOptions, :: DataPath, :: VerifiedValue, :: MaskedValue, :: Verification, :: InteractionMask
from iTasks.Framework.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks.Framework.Generic.Defaults		import generic gDefault
from iTasks.API.Core.Types	        import :: DateTime, :: User, :: Config, :: TaskId, :: TaskNo, :: InstanceNo, :: TaskListItem, :: TaskTime, :: SessionId
from iTasks.API.Core.SDSCombinators import sdsLens
from iTasks.API.Common.SDSCombinators import >+<, sdsFocus
from iTasks							import serialize, deserialize, functionFree
from System.Time 					import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp
from GenEq import generic gEq

instance toString StoreReadError
where
    toString StoreReadMissingError         = "Stored data not in store"
    toString StoreReadDataError            = "Failed to read store data"
    toString StoreReadTypeError            = "Stored data is of incorrect type"
    toString StoreReadBuildVersionError    = "Stored data contains functions from an older executable that can no longer be evaluated"

derive class iTask StoreReadError

//Temporary memory storage
memoryStore :: !StoreNamespace !(Maybe a) -> RWShared StoreName a a | TC a
memoryStore namespace defaultV = createReadWriteSDS namespace "memoryStore" read write
where
    read key iworld=:{IWorld|memoryShares}
        = case get (namespace,key) memoryShares of
            (Just (val :: a^))  = (Ok val,iworld)
            (Just _)            = (Error (exception StoreReadTypeError), iworld)
            _                   = case defaultV of
                Nothing     = (Error (exception StoreReadMissingError), iworld)
                Just val    = (Ok val, {IWorld|iworld & memoryShares = put (namespace,key) (dynamic val :: a^) memoryShares})
	write key val iworld=:{IWorld|memoryShares}
        = (Ok ((==) key),{IWorld|iworld & memoryShares = put (namespace,key) (dynamic val :: a^) memoryShares})

//'Core' file storage SDS
fullFileStore :: !StoreNamespace !Bool !(Maybe {#Char}) -> RWShared StoreName (!BuildID,!{#Char}) {#Char}
fullFileStore namespace resetOnError defaultV = createReadWriteSDS namespace "fullFileStore" read write
where
	read key iworld=:{IWorld|onClient,server={buildID}}
        | onClient //Special case for tasks running on a client
            # (mbVal,iworld) = jsLoadValue namespace key iworld
	        = (maybe (Error (exception StoreReadMissingError)) Ok mbVal, iworld)
	    # (mbItem,iworld) = readFromDisk namespace key iworld
	    = case (mbItem,defaultV) of
 		    (Ok item,_)
                = (Ok item,iworld)
            (Error StoreReadMissingError,Just def)
                # iworld = writeToDisk namespace key def iworld
                = (Ok (buildID,def),iworld)
            (Error e,Just def) | resetOnError
                # iworld = writeToDisk namespace key def iworld
                = (Ok (buildID,def),iworld)
            (Error e,Nothing) | resetOnError
                # iworld = deleteValue namespace key iworld
                = (Error (exception e), iworld)
		    (Error e,_)
                = (Error (exception e),iworld)

	write key value iworld=:{IWorld|onClient}
        | onClient //Special case for tasks running on a client
	        = (Ok ((==) key),jsStoreValue namespace key value iworld)
        | otherwise
	        = (Ok ((==) key),writeToDisk namespace key value iworld)

//Utility SDS which provides the current build such that higher level stores can check against it
buildID :: RWShared p BuildID Void
buildID = createReadWriteSDS "system" "buildID" read write
where
    read _ iworld=:{server={buildID}} = (Ok buildID,iworld)
    write _ _ iworld = (Ok (const False),iworld)

//Convenient derived store which checks version
jsonFileStore :: !StoreNamespace !Bool !Bool !(Maybe a) -> RWShared StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
jsonFileStore namespace checkBuild resetOnError defaultV
    = sdsLens "jsonFileStore" id (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify)
        (fullFileStore namespace resetOnError defaultJ >+< buildID)
where
    defaultJ = fmap (toString o toJSON) defaultV
    read key ((buildWhenStored,enc),buildNow)
        # json = fromString enc
        | checkBuild && (buildWhenStored <> buildNow && not (functionFree json))
            = Error (exception StoreReadBuildVersionError)
        | otherwise
            = case fromJSON json of
                Just v  = Ok v
                Nothing = Error (exception StoreReadTypeError)
    write key w = Ok (Just (toString (toJSON w),Void))
    notify key w = const True

cachedJSONFileStore :: !StoreNamespace !Bool !Bool !Bool !(Maybe a) -> RWShared StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
cachedJSONFileStore namespace checkBuild resetOnError keepBetweenEvals defaultV
    = createReadWriteSDS namespace "cachedJSONFileStore" read write
where
	read key iworld=:{IWorld|onClient,server={buildID},cachedShares}
        | onClient //Special case for tasks running on a client
            # (mbVal,iworld) = jsLoadValue namespace key iworld
	        = (maybe (Error (exception StoreReadMissingError)) Ok mbVal, iworld)
        //Try cache first
        # mbResult = case get (namespace,key) cachedShares of
            (Just (val :: a^,_,_))  = Just (Ok val)
            (Just _)                = Just (Error (exception StoreReadTypeError))
            Nothing                 = Nothing
        | mbResult =:(Just _)
            = (fromJust mbResult,iworld)
        //Try disk if the value is not in the cache
	    # (mbItem,iworld) = readFromDisk namespace key iworld
	    = case (mbItem,defaultV) of
 		    (Ok (buildIDWhenStored,encoded),_)
                # json = fromString encoded
                | checkBuild && (buildIDWhenStored <> buildID && not (functionFree json))
                    = (Error (exception StoreReadBuildVersionError),iworld)
                | otherwise
                    = case fromJSON json of
                        Just value
                            //Keep in cache
                            # iworld = {iworld & cachedShares = put (namespace,key) (dynamic value,keepBetweenEvals,Nothing) cachedShares}
                            = (Ok value,iworld)
                        Nothing = (Error (exception StoreReadTypeError),iworld)
            (Error StoreReadMissingError,Just def)
                # iworld = {iworld & cachedShares = put (namespace,key) (dynamic def, keepBetweenEvals,Just (DeferredJSON def)) cachedShares}
                = (Ok def,iworld)
            (Error e,Just def) | resetOnError
                # iworld = {iworld & cachedShares = put (namespace,key) (dynamic def, keepBetweenEvals,Just (DeferredJSON def)) cachedShares}
                = (Ok def,iworld)
            (Error e,Nothing) | resetOnError
                # iworld = deleteValue namespace key iworld
                = (Error (exception e), iworld)
		    (Error e,_)
                = (Error (exception e),iworld)

	write key value iworld=:{IWorld|onClient,cachedShares}
        | onClient //Special case for tasks running on a client
	        = (Ok ((==) key),jsStoreValue namespace key value iworld)
        | otherwise
            //Write to cache
            # iworld = {iworld & cachedShares = put (namespace,key) (dynamic value, keepBetweenEvals,Just (DeferredJSON value)) cachedShares}
	        = (Ok ((==) key),iworld)

flushShareCache :: *IWorld -> *IWorld
flushShareCache iworld=:{IWorld|onClient,cachedShares}
    | onClient = iworld
    | otherwise
        # (shares,iworld) = foldr flushShare ([],iworld) (toList cachedShares)
        = {iworld & cachedShares = fromList shares}
where
    flushShare cached=:((namespace,key),(val,keep,mbDeferredWrite)) (shares,iworld)
        # iworld = case mbDeferredWrite of
            Just deferred   = writeToDisk namespace key (toString (toJSON deferred)) iworld
            Nothing         = iworld
        | keep  = ([((namespace,key),(val,keep,Nothing)):shares],iworld)
                = (shares,iworld)

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
    = case readFromDisk namespace key iworld of
        (Ok (_,content),iworld) = (Ok content,iworld)
        (Error e,iworld) = (Error e,iworld)
	
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
	| not ok			= abort ("Failed to write value to store: " +++ filename +++ "\n") //TODO: USE ERROR INSTEAD OF ABORT
    //Write build ID
    # file              = fwrites buildID file
    //Write content
	# file				= fwrites content file
	# (ok,world)		= fclose file world
	= {IWorld|iworld & world = world}

readFromDisk :: !StoreNamespace !StoreName !*IWorld -> (MaybeError StoreReadError (!BuildID,!{#Char}), !*IWorld)	
readFromDisk namespace key iworld=:{server={paths={dataDirectory}},world}
	# filename			= addExtension (dataDirectory </> "stores" </> namespace </> safeName key) "txt"
	# (ok,file,world)	= fopen filename FReadData world
	| ok
	    # (content,file)	= freadfile file
		# (ok,world)		= fclose file world
        | ok
            # buildId = subString 0 15 content
            # content = dropChars 15 content
		    = (Ok (buildId,content), {iworld & world = world})
        | otherwise
            = (Error StoreReadDataError,{iworld & world = world})
    | otherwise
        = (Error StoreReadMissingError, {iworld & world = world})
where
	freadfile file = rec file ""
	where
		rec :: *File String -> (String, *File) //TODO: READ MORE EFFICIENT!
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

//Utility function to make sure we don't use names that escape the file path
safeName :: !String -> String
safeName s = copy 0 (createArray len '\0')
where
	len = size s
	copy :: !Int !*String -> String
	copy i n
		| i == len	= n
		| isAlphanum s.[i] || s.[i] == '-'  = copy (i + 1) {n & [i] = s.[i]}
							                = copy (i + 1) {n & [i] = '_'}

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

