implementation module iTasks.Framework.TaskStore

import StdEnv
import Data.Maybe, Text, System.Time, Math.Random, Text.JSON, Data.Func, Data.Tuple, Data.List, Data.Error, System.FilePath

import iTasks.Framework.IWorld, iTasks.Framework.TaskState, iTasks.Framework.Task, iTasks.Framework.Store
import iTasks.Framework.TaskEval, iTasks.Framework.Util, iTasks.Framework.UIDefinition
import iTasks.API.Core.SDSCombinators, iTasks.API.Common.SDSCombinators

from iTasks.Framework.SDS as SDS import qualified read, write, createReadWriteSDS

import iTasks.Framework.Serialization
import qualified Data.Map

derive gEq TIType

//Derives required for storage of UI definitions
derive JSONEncode TaskResult, TaskEvalInfo, TaskRep, TIValue
derive JSONEncode UIDef, UIContent, UIAction, UIViewport, UIWindow, UIControl, UIFSizeOpts, UISizeOpts, UIHSizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIProgressOpts, UISliderOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONEncode UIEmpty, UIForm, UIBlock
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONEncode UISize, UIBound, UIDirection, UIWindowType,  UIHAlign, UIVAlign, UISideSizes, UIMenuItem
derive JSONEncode UITaskletOpts, UIEditletOpts, UIEmbeddingOpts

derive JSONDecode TaskResult, TaskEvalInfo, TaskRep, TIValue
derive JSONDecode UIDef, UIContent, UIAction, UIViewport, UIWindow, UIControl, UIFSizeOpts, UISizeOpts, UIHSizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONDecode UIProgressOpts, UISliderOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONDecode UIEmpty, UIForm, UIBlock
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONDecode UISize, UIBound, UIDirection, UIWindowType, UIHAlign, UIVAlign, UISideSizes, UIMenuItem
derive JSONDecode UITaskletOpts, UIEditletOpts, UIEmbeddingOpts

INCREMENT				:== "increment"
SHARE_REGISTRATIONS		:== "share-registrations"

meta_index      = "instances"
rep_store t		= toString t +++ "-rep"
value_store t	= toString t +++ "-value"
reduct_store t	= toString t +++ "-reduct"

newInstanceNo :: !*IWorld -> (!InstanceNo,!*IWorld)
newInstanceNo iworld
	# (mbNewTid,iworld) = singleValueStoreRead NS_TASK_INSTANCES INCREMENT False iworld
	= case mbNewTid of
		Ok tid
			# iworld = singleValueStoreWrite NS_TASK_INSTANCES INCREMENT False (tid+1) iworld
			= (tid,iworld)
		Error e
			# iworld = singleValueStoreWrite NS_TASK_INSTANCES INCREMENT False 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)
			
maxInstanceNo :: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo iworld
	# (mbNewTid,iworld) = singleValueStoreRead NS_TASK_INSTANCES INCREMENT False iworld
	= case mbNewTid of
		Ok tid	= (tid-1,iworld)
		Error e = (0,iworld)

newInstanceKey :: !*IWorld -> (!InstanceKey, !*IWorld)
newInstanceKey iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop 32 random})

newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop 32 random})
	
deleteInstance	:: !InstanceNo !*IWorld -> *IWorld
deleteInstance instanceNo iworld
    //Delete all states
    # iworld        = deleteValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
    # iworld        = deleteValue NS_TASK_INSTANCES (value_store instanceNo) iworld
    # iworld=:{ti}  = deleteValue NS_TASK_INSTANCES (rep_store instanceNo) iworld
    = {iworld & ti = [m \\ m <- ti | m.TIMeta.instanceNo <> instanceNo]}

initInstanceMeta :: !*IWorld -> *IWorld
initInstanceMeta iworld
    # (mbIndex,iworld) = singleValueStoreRead NS_TASK_INSTANCES meta_index False iworld
    = case mbIndex of
        //When we restore the process index, reset all the connectedTo fields
        //because at startup we know there are no connections, but the table may be outdated
        //when the server was not shutdown properly
        Ok ti       = {iworld & ti = [{TIMeta|i & progress = {ProgressMeta|i.TIMeta.progress & connectedTo = Nothing}}  \\ i <- ti]}
        Error e     = {iworld & ti = []}

fullInstanceMeta :: RWShared Void [TIMeta] [TIMeta]
fullInstanceMeta = 'SDS'.createReadWriteSDS NS_TASK_INSTANCES "tiindex" read write
where
    read Void iworld=:{IWorld|ti}
		= (Ok ti, iworld)
    write Void ti iworld
        # iworld = {iworld & ti = ti}
        # iworld = singleValueStoreWrite NS_TASK_INSTANCES meta_index False ti iworld //Sync to disk to enable server restarts
        = (Ok (const True),iworld)

filteredInstanceMeta :: RWShared InstanceFilter [TIMeta] [TIMeta]
filteredInstanceMeta = sdsSplit (\p -> (Void,p)) read write fullInstanceMeta
where
    read tfilter is = filter (filterFun tfilter) is

    write tfilter is ws = let (ds,us) = splitWith (filterFun tfilter) is in
        (us ++ ws, notifyFun (ds ++ ws))

    filterFun {InstanceFilter|instanceNo,session} i
        =   (maybe True (\m -> i.TIMeta.instanceNo == m) instanceNo)
        &&  (maybe True (\m -> i.TIMeta.session == m) session)

    notifyFun ws qfilter = any (filterFun qfilter) ws

//The instance meta data is stored directly in the iworld
taskInstanceMeta :: RWShared InstanceNo TIMeta TIMeta
taskInstanceMeta = mapSingle (sdsTranslate (\no -> {InstanceFilter|instanceNo=Just no, session = Nothing}) filteredInstanceMeta)

//The remaining instance parts are stored on disk
taskInstanceReduct :: !InstanceNo -> RWShared Void TIReduct TIReduct
taskInstanceReduct instanceNo = singleValueStoreSDS NS_TASK_INSTANCES (reduct_store instanceNo) True False Nothing

taskInstanceValue :: !InstanceNo -> RWShared Void TIValue TIValue
taskInstanceValue instanceNo = singleValueStoreSDS NS_TASK_INSTANCES (value_store instanceNo) True False Nothing

taskInstanceRep :: !InstanceNo -> RWShared Void TaskRep TaskRep
taskInstanceRep instanceNo = singleValueStoreSDS NS_TASK_INSTANCES (rep_store instanceNo) True False Nothing

createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "?download="+++documentId, name = name, mime = mime, size = size content}
	# iworld				= blobStoreWrite NS_DOCUMENT_CONTENT (documentId +++ "-data") content iworld
	# iworld				= singleValueStoreWrite NS_DOCUMENT_CONTENT (documentId +++ "-meta") False document iworld	
	= (Ok document,iworld)
	
loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= case blobStoreRead NS_DOCUMENT_CONTENT (documentId +++ "-data") iworld of
        (Ok content,iworld) = (Just content,iworld)
        (Error e,iworld)    = (Nothing,iworld)

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= case (singleValueStoreRead NS_DOCUMENT_CONTENT (documentId +++ "-meta") False iworld) of
        (Ok doc,iworld)     = (Just doc,iworld)
        (Error e,iworld)    = (Nothing,iworld)

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{server={buildID,paths={dataDirectory}}}
	= (dataDirectory </>"stores"</> NS_DOCUMENT_CONTENT </> (documentId +++ "-data.txt"),iworld)

