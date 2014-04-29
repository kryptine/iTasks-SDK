implementation module iTasks.API.Extensions.Admin.StoreAdmin
import iTasks
import qualified iTasks.Framework.Store

manageStore :: Task Void
manageStore
    =   selectStore
    >^* [OnAction ActionDelete (hasValue deleteStore)]
    @! Void

selectStore :: Task (String,String) //Namespace and store name
selectStore
    =   (enterChoiceWithShared (Title "Namespace") [] storeNamespaces
    >&> \sNamespace -> whileUnchanged sNamespace
        \mbNamespace -> case mbNamespace of
            Nothing = enterChoice (Title "Stores") [ChooseWith (ChooseFromTree group)] []
            Just ns = enterChoiceWithShared (Title "Stores") [ChooseWith (ChooseFromTree group)] (sdsFocus ns storeNames) @ (\s -> (ns,s))
    ) <<@ (ArrangeWithSideBar 0 TopSide 55 False)
where
    group xs _ = [{ChoiceTree|label=x,icon=Nothing,value=ChoiceNode i,type=LeafNode} \\ (i,x) <- xs]

//Low-level access
deleteStore :: (String,String) -> Task ()
deleteStore (namespace,storename) = mkInstantTask eval
where
    eval _ iworld
        = (Ok (), 'iTasks.Framework.Store'.deleteValue namespace storename iworld)

