implementation module iTasks.Framework.TaskStore

import StdEnv
import Data.Maybe, Text, System.Time, Math.Random, Text.JSON, Data.Map, Data.Func, Data.Tuple, Data.Error, System.FilePath

import iTasks.Framework.IWorld, iTasks.Framework.TaskState, iTasks.Framework.Task, iTasks.Framework.Store
import iTasks.Framework.Util, iTasks.Framework.UIDefinition

from Data.SharedDataSource import qualified read, write, mapReadWriteError, createChangeOnWriteSDS
import iTasks.Framework.SerializationGraphCopy //TODO: Make switchable from within iTasks module
import qualified Data.Map

//Derives required for storage of UI definitions
derive JSONEncode TaskResult, TaskInfo, TaskRep, TIValue
derive JSONEncode UIDef, UIContent, UIAction, UIViewport, UIWindow, UIControl, UIFSizeOpts, UISizeOpts, UIHSizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONEncode UIProgressOpts, UISliderOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONEncode UIControlStack, UISubUI, UISubUIStack
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONEncode UISize, UIBound, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem
derive JSONEncode UITaskletOpts, UIEditletOpts

derive JSONDecode TaskResult, TaskInfo, TaskRep, TIValue
derive JSONDecode UIDef, UIContent, UIAction, UIViewport, UIWindow, UIControl, UIFSizeOpts, UISizeOpts, UIHSizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UIItemsOpts
derive JSONDecode UIProgressOpts, UISliderOpts, UIGridOpts, UITreeOpts, UIIconOpts, UILabelOpts, UITreeNode
derive JSONDecode UIControlStack, UISubUI, UISubUIStack
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UIViewportOpts, UITabSetOpts, UITab, UITabOpts
derive JSONDecode UISize, UIBound, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem
derive JSONDecode UITaskletOpts, UIEditletOpts

INCREMENT				:== "increment"
SHARE_REGISTRATIONS		:== "share-registrations"

meta_store t    = toString t +++ "-meta"
rep_store t		= toString t +++ "-rep"
value_store t	= toString t +++ "-value"
reduct_store t	= toString t +++ "-reduct"

newInstanceNo :: !*IWorld -> (!InstanceNo,!*IWorld)
newInstanceNo iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT (tid+1) iworld
			= (tid,iworld)
		Nothing
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)
			
maxInstanceNo :: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid	= (tid-1,iworld)
		Nothing		= (0,iworld)

newInstanceKey :: !*IWorld -> (!InstanceKey, !*IWorld)
newInstanceKey iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop 32 random})

newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop 32 random})
	
deleteInstance	:: !InstanceNo !*IWorld -> *IWorld
deleteInstance instanceNo iworld
    = case 'Data.SharedDataSource'.read fullInstanceMeta iworld of
		(Ok list,iworld)    = snd ('Data.SharedDataSource'.write (del instanceNo list) fullInstanceMeta iworld)
		(_,iworld)          = iworld

initInstanceMeta :: !*IWorld -> *IWorld
initInstanceMeta iworld
    # (keys,iworld) = listKeys NS_TASK_INSTANCES iworld
    # (ti,iworld)   = foldl restore (newMap,iworld) [key \\ key <- keys | endsWith "-meta" key]
    = {iworld & ti = ti}
where
    restore (ti,iworld) key
        # instanceNo = toInt (hd (split "-" key))
        # (mbMeta,iworld) = loadValue NS_TASK_INSTANCES key iworld
        = (maybe ti (\meta -> 'Data.Map'.put instanceNo meta ti) mbMeta,iworld)

fullInstanceMeta :: RWShared (Map InstanceNo TIMeta) (Map InstanceNo TIMeta) IWorld
fullInstanceMeta = 'Data.SharedDataSource'.createChangeOnWriteSDS NS_TASK_INSTANCES "meta-index" read write
where
    read iworld=:{IWorld|ti}
		= (Ok ti, iworld)
    write ti iworld
        # iworld = {iworld & ti = ti}
        = (Ok Void,iworld)

//The instance meta data is stored directly in the iworld
taskInstanceMeta :: !InstanceNo -> RWShared TIMeta TIMeta IWorld
taskInstanceMeta instanceNo = 'Data.SharedDataSource'.createChangeOnWriteSDS NS_TASK_INSTANCES (meta_store instanceNo) read write
where
    read iworld=:{IWorld|ti}
		= (maybe (Error ("Could not read task instance meta of instance "+++toString instanceNo)) Ok ('Data.Map'.get instanceNo ti), iworld)
	write meta iworld=:{IWorld|ti}
        # iworld = {iworld & ti = 'Data.Map'.put instanceNo meta ti}
        # iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) meta iworld //Sync to disk to enable server restarts
        = (Ok Void,iworld)

//The remaining instance parts are stored on disk
taskInstanceReduct :: !InstanceNo -> RWShared TIReduct TIReduct IWorld
taskInstanceReduct instanceNo = storeAccess NS_TASK_INSTANCES (reduct_store instanceNo) Nothing

taskInstanceValue :: !InstanceNo -> RWShared TIValue TIValue IWorld
taskInstanceValue instanceNo = storeAccess NS_TASK_INSTANCES (value_store instanceNo) Nothing

taskInstanceRep :: !InstanceNo -> RWShared TaskRep TaskRep IWorld
taskInstanceRep instanceNo = storeAccess NS_TASK_INSTANCES (rep_store instanceNo) Nothing

createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "?download="+++documentId, name = name, mime = mime, size = size content}
	# iworld				= storeBlob NS_DOCUMENT_CONTENT (documentId +++ "-data") content iworld
	# iworld				= storeValue NS_DOCUMENT_CONTENT (documentId +++ "-meta") document iworld	
	= (Ok document,iworld)
	
createDocumentWith :: !String !String (*File -> *File) !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocumentWith name mime f iworld 
	= createDocument name mime "FIXME" iworld //TODO make it possible to apply the function during creation
	
loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= loadBlob NS_DOCUMENT_CONTENT (documentId +++ "-data") iworld

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= loadValue NS_DOCUMENT_CONTENT (documentId +++ "-meta") iworld

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{server={buildID,paths={dataDirectory}}}
	= (storePath dataDirectory buildID </> NS_DOCUMENT_CONTENT </> (documentId +++ "_data.bin"),iworld)

addShareRegistration :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
addShareRegistration shareId instanceNo iworld
	# (mbRegs,iworld) = loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs	= fromMaybe newMap mbRegs
	# sregs	= fromMaybe [] (get shareId regs)
	# regs	= put shareId (removeDup (sregs ++ [instanceNo])) regs
	= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
	
clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= maybe newMap (fromList o clear instanceNo o toList) mbRegs
	= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
where
	clear :: InstanceNo [(BasicShareId,[InstanceNo])] -> [(BasicShareId,[InstanceNo])]
	clear no regs = [(shareId,removeMember no insts) \\ (shareId,insts) <- regs]

addOutdatedOnShareChange :: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
addOutdatedOnShareChange shareId filterFun iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= fromMaybe newMap mbRegs
	= case get shareId regs of
		Just outdated=:[_:_]
			# iworld			= addOutdatedInstances [(outd, Nothing) \\ outd <- filter filterFun outdated] iworld
			# regs				= put shareId (filter (not o filterFun) outdated) regs
			= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
		_	= iworld

addOutdatedInstances :: ![(!InstanceNo, !Maybe Timestamp)] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = seqSt queueWork [(Evaluate instanceNo,mbTs) \\ (instanceNo,mbTs) <- outdated] iworld

