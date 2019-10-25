implementation module iTasks.Extensions.Admin.StoreAdmin

import iTasks
import qualified iTasks.Internal.Store
import qualified iTasks.Internal.Task
import Data.Error

manageStore :: Task ()
manageStore
    =   selectStore
    >^* [OnAction ActionDelete (hasValue deleteStore)]
    @! ()

selectStore :: Task (String,String) //Namespace and store name
selectStore
    =   (enterChoiceWithShared [] storeNamespaces <<@ Title "Namespace"
    >&> \sNamespace -> whileUnchanged sNamespace
        \mbNamespace -> case mbNamespace of
            Nothing = enterChoice [ChooseFromGrid id] [] <<@ Title "Store"
            Just ns = enterChoiceWithShared [ChooseFromGrid id] (sdsFocus ns storeNames) <<@ Title "Stores" @ (\s -> (ns,s))
    ) <<@ ArrangeWithSideBar 0 TopSide False

//Low-level access
deleteStore :: (String,String) -> Task ()
deleteStore (namespace,storename) = 'iTasks.Internal.Task'.mkInstantTask eval
where
    eval _ iworld = case 'iTasks.Internal.Store'.deleteValue namespace storename iworld of
		(Ok (),iworld) = (Ok (),iworld)
		(Error msg,iworld) = (Error (exception msg),iworld)
