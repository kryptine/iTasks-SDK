implementation module iTasks.API.Common.SDSCombinators

import StdTuple, StdList
import iTasks.API.Core.SDSs, iTasks.API.Core.SDSCombinators
import iTasks.API.Core.Types
from StdFunc import o, const, flip, id
from iTasks._Framework.Task import exception

sdsFocus :: !p !(RWShared p r w) -> (RWShared p` r w) | iTask p
sdsFocus p sds = sdsTranslate ("("+++ toString (toJSON p)+++")/") (const p) sds

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w | iTask p
sdsProject read write sds = sdsLens "()" param` read` write` (SDSNotifyConst notify`) sds
where
    param` p = p
    read` = case read of
        SDSLensRead f = SDSRead (\p rs -> f rs)
        SDSConstRead f = SDSReadConst (\_ -> f)

    write` = case write of
        SDSLensWrite f = SDSWrite (\p rs w -> f rs w)
        SDSBlindWrite f = SDSWriteConst (\p w -> f w)
        SDSNoWrite      = SDSWriteConst (\p w -> Ok Nothing)
    notify` p w = const True

sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | iTask ps
sdsTranslate name param sds = sdsLens name param (SDSRead (\_ rs -> Ok rs)) (SDSWriteConst (\_ w -> Ok (Just w))) (SDSNotifyConst (\_ _ _ -> True)) sds

sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | iTask ps & iTask pn
sdsSplit name param read write sds = sdsLens name param` (SDSRead read`) (SDSWrite write`) (SDSNotify notify`) sds
where
    param` p            = fst (param p)
    read` p rs          = Ok (read (snd (param p)) rs)
    write` p rs w       = Ok (Just (fst (write (snd (param p)) rs w)))
    notify` p rs w pq   = (snd (write (snd (param p)) rs w)) (snd (param pq))

mapRead :: !(r -> r`) !(RWShared p r w) -> RWShared p r` w | iTask p
mapRead read sds = mapReadError (\r -> Ok (read r)) sds

mapWrite :: !(w` r -> Maybe w) !(RWShared p r w) -> RWShared p r w` | iTask p
mapWrite write sds = mapWriteError (\r w -> Ok (write r w)) sds

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared p r w) -> RWShared p r` w` | iTask p
mapReadWrite (read,write) sds = mapReadWriteError (\r -> Ok (read r), (\r w -> Ok (write r w))) sds

mapReadError :: !(r -> MaybeError TaskException r`) !(RWShared p r w) -> RWShared p r` w | iTask p
mapReadError read sds = sdsProject (SDSLensRead read) (SDSBlindWrite (Ok o Just)) sds

mapWriteError :: !(w` r -> MaybeError TaskException  (Maybe w)) !(RWShared p r w) -> RWShared p r w` | iTask p
mapWriteError write sds = sdsProject (SDSLensRead Ok) (SDSLensWrite (flip write)) sds
	
mapReadWriteError :: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w)) !(RWShared p r w) -> RWShared p r` w` | iTask p
mapReadWriteError (read,write) sds = sdsProject (SDSLensRead read) (SDSLensWrite (flip write)) sds

mapSingle :: !(RWShared p [r] [w]) -> (RWShared p r w) | iTask p
mapSingle sds = sdsProject (SDSLensRead read) (SDSBlindWrite write) sds
where
    read [x]    = Ok x
    read []     = Error (exception "List element not found")
    read _      = Error (exception "Multiple list elements found, expected only one")

    write x     = Ok (Just [x])

toReadOnly :: !(RWShared p r w) -> ROShared p r | iTask p
toReadOnly sds = sdsProject (SDSLensRead Ok) SDSNoWrite sds

toDynamic :: !(RWShared p r w) -> (RWShared p Dynamic Dynamic) | iTask p & TC r & TC w //FIXME: Use 1 lens directly
toDynamic sds = mapRead (\r -> (dynamic r :: r^)) (mapWrite (\(w :: w^) _ -> Just w) sds) 

(>+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy) | iTask p
(>+<) sds1 sds2 = sdsParallel ">+<" (\p -> (p,p)) id (SDSWriteConst write1) (SDSWriteConst write2) sds1 sds2
where
    write1 _ w = Ok (Just (fst w))
    write2 _ w = Ok (Just (snd w))

(>+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx | iTask p
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, ())) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy | iTask p
(|+<) srcX srcY = mapWrite (\wy _ -> Just ((), wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) () | iTask p
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)

symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared p a a) !(RWShared p b b) -> (!RWShared p a a, !RWShared p b b) | iTask p
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

//Derived shares of tasklists
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
taskListState tasklist = mapRead (\(_,items) -> [value \\ {TaskListItem|value} <- items]) (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}

taskListMeta :: !(SharedTaskList a) -> ReadWriteShared [TaskListItem a] [(TaskId,TaskAttributes)]
taskListMeta tasklist = mapRead (\(_,items) -> items) (sdsFocus listFilter tasklist)
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}

taskListIds :: !(SharedTaskList a) -> ROShared () [TaskId]
taskListIds tasklist = mapRead prj (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
    prj (_,items) = [taskId \\ {TaskListItem|taskId} <- items]

taskListEntryMeta :: !(SharedTaskList a) -> RWShared TaskId (TaskListItem a) TaskAttributes
taskListEntryMeta tasklist = mapSingle (sdsSplit "taskListEntryMeta" param read write tasklist)
where
    param p = ({onlyIndex=Nothing,onlyTaskId=Just [p],onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True},p)
    read p (_,items) = [i \\ i=:{TaskListItem|taskId} <- items | taskId == p]
    write p _ attributes    = ([(p,a) \\ a <- attributes], (==) p)

taskListSelfId :: !(SharedTaskList a) -> ReadOnlyShared TaskId
taskListSelfId tasklist = mapRead (\(_,items) -> hd [taskId \\ {TaskListItem|taskId,self} <- items | self]) (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=True,includeValue=False,includeAttributes=False,includeProgress=False}

taskListSelfManagement :: !(SharedTaskList a) -> Shared TaskAttributes
taskListSelfManagement tasklist = mapReadWriteError (toPrj,fromPrj) (sdsFocus listFilter tasklist)
where
    toPrj (_,items) = case [m \\ m=:{TaskListItem|taskId,self} <- items | self] of
        []                              = Error (exception "Task id not found in self management share")
        [{TaskListItem|attributes}:_]   = Ok attributes

    fromPrj attributes (_,[{TaskListItem|taskId}])
        = Ok (Just [(taskId,attributes)])

    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=True,includeValue=False,includeAttributes=True,includeProgress=False}

taskListItemValue :: !(SharedTaskList a) -> ROShared (Either Int TaskId) (TaskValue a)
taskListItemValue tasklist = mapReadError read (toReadOnly (sdsTranslate "taskListItemValue" listFilter tasklist))
where
    listFilter (Left index) = {onlyIndex=Just [index],onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}
    listFilter (Right taskId) = {onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}

    read (_,items) = case [value \\ {TaskListItem|value} <- items] of
        [v:_]   = Ok v
        _       = Error (exception "taskListItemValue: item not found")

taskListItemProgress :: !(SharedTaskList a) -> ROShared (Either Int TaskId) InstanceProgress
taskListItemProgress tasklist = mapReadError read (toReadOnly (sdsTranslate "taskListItemProgress" listFilter tasklist))
where
    listFilter (Left index) = {onlyIndex=Just [index],onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=True}
    listFilter (Right taskId) = {onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=True}

    read (_,items) = case [p \\ {TaskListItem|progress=Just p} <- items] of
        [p:_]   = Ok p
        _       = Error (exception "taskListItemProgress: item not found")


