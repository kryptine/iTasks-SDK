implementation module iTasks._Framework.Store

import StdEnv
import Data.Void
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Maybe, Data.Functor, Data.Error
import System.File, System.Directory, System.OSError, System.FilePath
import Text, Text.JSON

import iTasks._Framework.Client.JSStore
import iTasks._Framework.SDS

from iTasks._Framework.IWorld		import :: IWorld {onClient,server,memoryShares,cachedShares,world}, :: ServerInfo(..), :: SystemPaths(..), :: Resource, :: ShareCache(..)
from iTasks._Framework.Task		    import exception
from iTasks._Framework.TaskState		import :: DeferredJSON(..)
from iTasks._Framework.Generic				import class iTask
from iTasks._Framework.Generic.Interaction	import generic gEditor, generic gVerify, :: VSt, ::USt, :: VerifyOptions, :: DataPath, :: VerifiedValue, :: Verification 
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks._Framework.Generic.Defaults		import generic gDefault
from iTasks.API.Core.Types	        import :: DateTime, :: Config, :: TaskId, :: TaskNo, :: InstanceNo, :: TaskListItem, :: TaskTime, :: SessionId
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
        = case 'DM'.get (namespace,key) memoryShares of
            (Just (val :: a^))  = (Ok val,iworld)
            (Just _)            = (Error (exception StoreReadTypeError), iworld)
            _                   = case defaultV of
                Nothing     = (Error (exception StoreReadMissingError), iworld)
                Just val    = (Ok val, {IWorld|iworld & memoryShares = 'DM'.put (namespace,key) (dynamic val :: a^) memoryShares})
	write key val iworld=:{IWorld|memoryShares}
        = (Ok ((==) key),{IWorld|iworld & memoryShares = 'DM'.put (namespace,key) (dynamic val :: a^) memoryShares})

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
                # (mbErr,iworld) = writeToDisk namespace key def iworld
				| mbErr =: (Error _)
					= (Error (exception (fromError mbErr)),iworld)
                = (Ok (buildID,def),iworld)
            (Error e,Just def) | resetOnError
                # (mbErr,iworld) = writeToDisk namespace key def iworld
				| mbErr =: (Error _)
					= (Error (exception (fromError mbErr)),iworld)
                = (Ok (buildID,def),iworld)
            (Error e,Nothing) | resetOnError
                # (_,iworld) = deleteValue namespace key iworld //Try to delete the value
                = (Error (exception e), iworld)
		    (Error e,_)
                = (Error (exception e),iworld)

	write key value iworld=:{IWorld|onClient}
        | onClient //Special case for tasks running on a client
	        = (Ok ((==) key),jsStoreValue namespace key value iworld)
        | otherwise
			# (mbErr,iworld) = writeToDisk namespace key value iworld
			| mbErr =: (Error _)
				= (Error (exception (fromError mbErr)),iworld)
	        = (Ok ((==) key),iworld)

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
        # mbResult = case 'DM'.get (namespace,key) cachedShares of
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
                            # iworld = {iworld & cachedShares = 'DM'.put (namespace,key) (dynamic value,keepBetweenEvals,Nothing) cachedShares}
                            = (Ok value,iworld)
                        Nothing = (Error (exception StoreReadTypeError),iworld)
            (Error StoreReadMissingError,Just def)
                # iworld = {iworld & cachedShares = 'DM'.put (namespace,key) (dynamic def, keepBetweenEvals,Just (DeferredJSON def)) cachedShares}
                = (Ok def,iworld)
            (Error e,Just def) | resetOnError
                # iworld = {iworld & cachedShares = 'DM'.put (namespace,key) (dynamic def, keepBetweenEvals,Just (DeferredJSON def)) cachedShares}
                = (Ok def,iworld)
            (Error e,Nothing) | resetOnError
                # (_,iworld) = deleteValue namespace key iworld //Try to delete value
                = (Error (exception e), iworld)
		    (Error e,_)
                = (Error (exception e),iworld)

	write key value iworld=:{IWorld|onClient,cachedShares}
        | onClient //Special case for tasks running on a client
	        = (Ok ((==) key),jsStoreValue namespace key value iworld)
        | otherwise
            //Write to cache
            # iworld = {iworld & cachedShares = 'DM'.put (namespace,key) (dynamic value, keepBetweenEvals,Just (DeferredJSON value)) cachedShares}
	        = (Ok ((==) key),iworld)

flushShareCache :: *IWorld -> *IWorld //TODO: Propagate error up
flushShareCache iworld=:{IWorld|onClient,cachedShares}
    | onClient = iworld
    | otherwise
        # (shares,iworld) = foldr flushShare ([],iworld) ('DM'.toList cachedShares)
        = {iworld & cachedShares = 'DM'.fromList shares}
where
    flushShare cached=:((namespace,key),(val,keep,mbDeferredWrite)) (shares,iworld)
        # iworld = case mbDeferredWrite of
            Just deferred   
				# (_,iworld) = writeToDisk namespace key (toString (toJSON deferred)) iworld
				= iworld
            Nothing         = iworld
        | keep  = ([((namespace,key),(val,keep,Nothing)):shares],iworld)
                = (shares,iworld)

blobStoreWrite :: !StoreNamespace !StoreName !{#Char} !*IWorld -> *IWorld //TODO: Propagate error up
blobStoreWrite namespace key blob iworld=:{IWorld|onClient=True}
	= jsStoreValue namespace key blob iworld
blobStoreWrite namespace key blob iworld
	# (_,iworld) = writeToDisk namespace key blob iworld 
	= iworld

blobStoreRead :: !StoreNamespace !StoreName !*IWorld -> (!MaybeError StoreReadError {#Char}, !*IWorld)
blobStoreRead namespace key iworld=:{onClient=True}
	# (mbBlob,iworld) =jsLoadValue namespace key iworld
    = (maybe (Error StoreReadMissingError) Ok mbBlob, iworld)
blobStoreRead namespace key iworld
    = case readFromDisk namespace key iworld of
        (Ok (_,content),iworld) = (Ok content,iworld)
        (Error e,iworld) = (Error e,iworld)
	
writeToDisk :: !StoreNamespace !StoreName !String !*IWorld -> (MaybeErrorString (), *IWorld)
writeToDisk namespace key content iworld=:{server={buildID,paths={dataDirectory}},world}
	# location = dataDirectory </> "stores"
	//Check if the location exists and create it otherwise
	# (exists,world)	= fileExists location world
	# (res,world)		= if exists (Ok (),world)
							( case createDirectory location world of
								(Ok Void, world) = (Ok (),world)
								(Error e, world) = (Error ("Cannot create store: " +++ location +++ ": " +++ snd e), world)
							)
	| res =: (Error _)
		= (res,{IWorld|iworld & world = world})
	//Check if the namespace exists and create it otherwise
	# (exists,world)	= fileExists (location </> namespace) world
	# (res,world)		= if exists (Ok (),world)
							( case createDirectory (location </> namespace) world of
								(Ok Void, world) = (Ok (), world)
								(Error e, world) = (Error ("Cannot create namespace " +++ namespace +++ ": " +++ snd e), world)
							)
	| res =: (Error _)
		= (res,{IWorld|iworld & world = world})
	//Write the value
	# filename 			= addExtension (location </> namespace </> safeName key) "txt"
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (Error ("Failed to write value to store: " +++ filename),{IWorld|iworld & world = world})
    //Write build ID
    # file              = fwrites buildID file
    //Write content
	# file				= fwrites content file
	# (ok,world)		= fclose file world
	= (Ok (),{IWorld|iworld & world = world})

readFromDisk :: !StoreNamespace !StoreName !*IWorld -> (MaybeError StoreReadError (!BuildID,!String), !*IWorld)	
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

deleteValue :: !StoreNamespace !StoreName !*IWorld -> *(MaybeErrorString (),*IWorld)
deleteValue namespace delKey iworld=:{onClient=True}
	= (Ok (), jsDeleteValue namespace delKey iworld)
deleteValue namespace delKey iworld = deleteValues` namespace delKey (==) filterFuncDisk iworld
where
	// compare key with filename without extension
	filterFuncDisk delKey key = dropExtension key == delKey

deleteValues :: !StoreNamespace !StorePrefix !*IWorld -> *(MaybeErrorString (),*IWorld)
deleteValues namespace delKey iworld = deleteValues` namespace delKey startsWith startsWith iworld

deleteValues` :: !String !String !(String String -> Bool) !(String String -> Bool) !*IWorld -> *(MaybeErrorString (),*IWorld)
deleteValues` namespace delKey filterFuncCache filterFuncDisk iworld=:{server={buildID,paths={dataDirectory}},world}
	//Delete items from disk
	# (res,world) = deleteFromDisk world
	= (res,{iworld & world = world})
where
	deleteFromDisk world
		# storeDir		= dataDirectory </>"stores"</> namespace
		# (res, world)	= readDirectory storeDir world
		= case res of
			(Ok _) = deleteFiles storeDir (fromOk res) world
			(Error (errNo,errMsg))
				| errNo == 2 //The store directory doesn't exist -> nothing to do
					= (Ok (), world)
				| otherwise
					= (Error ("Problem reading store directory " +++ storeDir +++ ": " +++ toString errNo +++ " " +++ errMsg),world)

	deleteFiles _ [] world
		= (Ok (),world)
	deleteFiles dir [f:fs] world
		| filterFuncDisk (safeName delKey) f
			# (res,world) = deleteFile (dir </> f) world
			= case res of
				(Ok _) = deleteFiles dir fs world
				(Error (errNo,errMsg))
					| errNo == 2 // The file doesn't exits -> nothing to do
						= deleteFiles dir fs world
					| otherwise
						= (Error ("Problem deleting store file " +++ (dir </> f) +++ ": " +++ toString errNo +++ " " +++ errMsg), world)
		| otherwise //Skip
			= deleteFiles dir fs world

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

emptyStore :: !*IWorld -> *IWorld
emptyStore iworld 
	# (namespaces,iworld) = listStoreNamespaces iworld
	= foldl (\w ns -> emptyNamespace ns w) iworld namespaces
where
	emptyNamespace ns iworld = case listStoreNames ns iworld of
		(Ok stores,iworld) = foldl (\w s -> snd (deleteValue ns s w)) iworld stores
		(Error _,iworld) = iworld
