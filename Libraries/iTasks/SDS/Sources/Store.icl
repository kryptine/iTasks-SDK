implementation module iTasks.SDS.Sources.Store

import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common

import iTasks.Internal.SDS
import iTasks.Internal.Store
import iTasks.Internal.Task
import iTasks.Internal.Serialization

sharedDynamicStore :: !String !a -> SDS () a a | TC a
sharedDynamicStore storeId defaultV
	= mapReadWriteError (read, write) (sharedStore storeId (dynamic defaultV))
where
	read (r :: a^) = r
	read x = Error (exception "Dynamic types mismatched?")

	write _ w = Ok (Just (dynamic w))

sharedStore :: !String !a -> SDS () a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV
    = sdsFocus storeId (jsonFileStore NS_APPLICATION_SHARES True True (Just defaultV))

storeNamespaces :: SDS () [String] ()
storeNamespaces = createReadOnlySDS read
where
    read () iworld = listStoreNamespaces iworld

storeNames :: SDS String [String] ()
storeNames = createReadOnlySDSError read
where
    read namespace iworld = case listStoreNames namespace iworld of
        (Ok names,iworld) = (Ok names,iworld)
        (Error e,iworld) = (Error (exception e),iworld)
