implementation module iTasks.SDS.Combinators.Common

import StdTuple, StdList
import iTasks.SDS.Definition, iTasks.SDS.Combinators.Core, iTasks.SDS.Sources.Core
import iTasks.WF.Combinators.Core
from StdFunc import o, const, flip, id
from iTasks.Internal.Task import exception, :: TaskException
import qualified Data.Map as DM
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
import Data.Maybe, Data.Error, Data.Either, StdString
import Text.GenJSON
import System.FilePath
import iTasks.Internal.SDS

sdsFocus :: !p !(sds p r w) -> (SDSLens p` r w) | gText{|*|} p & TC p & TC r & TC w & RWShared sds
sdsFocus p sds = sdsTranslate ("("+++ toString (toJSON p)+++")/") (const p) sds

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(sds p rs ws) -> SDSLens p r w | gText{|*|} p & TC p & TC rs & TC ws & RWShared sds
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

sdsTranslate :: !String !(p -> ps) !(sds ps r w) -> SDSLens p r w | gText{|*|} ps & TC ps & TC r & TC w & RWShared sds
sdsTranslate name param sds = sdsLens name param (SDSRead (\_ rs -> Ok rs)) (SDSWriteConst (\_ w -> Ok (Just w))) (SDSNotifyConst (\_ _ _ -> True)) sds

sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(sds ps rs ws) -> SDSLens p r w | gText{|*|} ps & TC ps & gText{|*|} pn & TC pn & TC rs  & TC ws & RWShared sds
sdsSplit name param read write sds = sdsLens name param` (SDSRead read`) (SDSWrite write`) (SDSNotify notify`) sds
where
    param` p            = fst (param p)
    read` p rs          = Ok (read (snd (param p)) rs)
    write` p rs w       = Ok (Just (fst (write (snd (param p)) rs w)))
    notify` p rs w pq   = (snd (write (snd (param p)) rs w)) (snd (param pq))

removeMaybe :: !(Maybe a) !(sds p (Maybe a) (Maybe a)) -> SDSLens p a a | gText{|*|} p & TC p & TC a & RWShared sds
removeMaybe defaultValue sds = sdsLens "removeMaybe" id (SDSRead read) (SDSWriteConst write) (SDSNotifyConst (\_ _ _ -> True)) sds
where
	read p (Just r) = Ok r
	read p Nothing = maybe (Error (exception "Required value not available in shared data source")) Ok defaultValue
	
	write p w = Ok (Just (Just w))

mapRead :: !(r -> r`) !(sds p r w) -> SDSLens p r` w | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapRead read sds = mapReadError (\r -> Ok (read r)) sds

mapWrite :: !(w` r -> Maybe w) !(sds p r w) -> SDSLens p r w` |gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapWrite write sds = mapWriteError (\r w -> Ok (write r w)) sds

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(sds p r w) -> SDSLens p r` w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapReadWrite (read,write) sds = mapReadWriteError (\r -> Ok (read r), (\r w -> Ok (write r w))) sds

mapReadError :: !(r -> MaybeError TaskException r`) !(sds p r w) -> SDSLens p r` w | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapReadError read sds = sdsProject (SDSLensRead read) (SDSBlindWrite (Ok o Just)) sds

mapWriteError :: !(w` r -> MaybeError TaskException  (Maybe w)) !(sds p r w) -> SDSLens p r w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapWriteError write sds = sdsProject (SDSLensRead Ok) (SDSLensWrite (flip write)) sds
	
mapReadWriteError :: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w)) !(sds p r w) -> SDSLens p r` w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapReadWriteError (read,write) sds = sdsProject (SDSLensRead read) (SDSLensWrite (flip write)) sds

mapSingle :: !(sds p [r] [w]) -> (SDSLens p r w) | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapSingle sds = sdsProject (SDSLensRead read) (SDSBlindWrite write) sds
where
    read [x]    = Ok x
    read []     = Error (exception "List element not found")
    read _      = Error (exception "Multiple list elements found, expected only one")

    write x     = Ok (Just [x])

toReadOnly :: !(sds p r w) -> SDSLens p r () | gText{|*|} p & TC p & TC r & TC w & RWShared sds
toReadOnly sds = sdsProject (SDSLensRead Ok) SDSNoWrite sds

toDynamic :: !(sds p r w) -> (SDSLens p Dynamic Dynamic) | gText{|*|} p & TC p & TC r & TC w & RWShared sds //FIXME: Use 1 lens directly
toDynamic sds = mapRead (\r -> (dynamic r :: r^)) (mapWrite (\(w :: w^) _ -> Just w) sds)

(>*<) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSParallel p (rx,ry) (wx,wy)     | gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & RWShared sds1 & RWShared sds2
(>*<) l r = sdsParallel ">+<" (\p -> (p,p)) id (SDSWriteConst write1) (SDSWriteConst write2) l r
  where
    write1 _ w = Ok (Just (fst w))
    write2 _ w = Ok (Just (snd w))

(>*|) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSLens p (rx,ry) wx          | gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & RWShared sds1 & RWShared sds2
(>*|) l r = mapWrite (\wx _ -> Just (wx, ())) (l >*< toReadOnly r)

(|*<) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSLens p (rx,ry) wy          | gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & RWShared sds1 & RWShared sds2
(|*<) l r = mapWrite (\wy _ -> Just ((), wy)) (toReadOnly l >*< r)

(|*|) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSLens p (rx,ry) ()          | gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & RWShared sds1 & RWShared sds2
(|*|) l r = toReadOnly (l >*< r)

symmetricLens :: !(a b -> b) !(b a -> a) !(sds1 p a a) !(sds2 p b b) -> (!SDSLens p a a, !SDSLens p b b) | gText{|*|} p & TC p & TC a & TC b & RWShared sds1 & RWShared sds2
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >*< sharedB
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

//Derived shares of tasklists
taskListState :: !(SharedTaskList a) -> SDSLens () [TaskValue a] () | TC a
taskListState tasklist = mapRead (\(_,items) -> [value \\ {TaskListItem|value} <- items]) (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}

taskListMeta :: !(SharedTaskList a) -> SDSLens () [TaskListItem a] [(TaskId,TaskAttributes)] | TC a
taskListMeta tasklist = mapRead (\(_,items) -> items) (sdsFocus listFilter tasklist)
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}

taskListIds :: !(SharedTaskList a) -> SDSLens () [TaskId] () | TC a
taskListIds tasklist = mapRead prj (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
    prj (_,items) = [taskId \\ {TaskListItem|taskId} <- items]

taskListEntryMeta :: !(SharedTaskList a) -> SDSLens TaskId (TaskListItem a) TaskAttributes | TC a
taskListEntryMeta tasklist = mapSingle (sdsSplit "taskListEntryMeta" param read write tasklist)
where
    param p = ({onlyIndex=Nothing,onlyTaskId=Just [p],onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True},p)
    read p (_,items) = [i \\ i=:{TaskListItem|taskId} <- items | taskId == p]
    write p _ attributes    = ([(p,a) \\ a <- attributes], (==) p)

taskListSelfId :: !(SharedTaskList a) -> SDSLens () TaskId () | TC a
taskListSelfId tasklist = mapRead (\(_,items) -> hd [taskId \\ {TaskListItem|taskId,self} <- items | self]) (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=True,includeValue=False,includeAttributes=False,includeProgress=False}

taskListSelfManagement :: !(SharedTaskList a) -> SDSLens () TaskAttributes TaskAttributes | TC a
taskListSelfManagement tasklist = mapReadWriteError (toPrj,fromPrj) (sdsFocus listFilter tasklist)
where
    toPrj (_,items) = case [m \\ m=:{TaskListItem|taskId,self} <- items | self] of
        []                              = Error (exception "Task id not found in self management share")
        [{TaskListItem|attributes}:_]   = Ok attributes

    fromPrj attributes (_,[{TaskListItem|taskId}])
        = Ok (Just [(taskId,attributes)])

    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=True,includeValue=False,includeAttributes=True,includeProgress=False}

taskListItemValue :: !(SharedTaskList a) -> SDSLens (Either Int TaskId) (TaskValue a) () | TC a
taskListItemValue tasklist = mapReadError read (toReadOnly (sdsTranslate "taskListItemValue" listFilter tasklist))
where
    listFilter (Left index) = {onlyIndex=Just [index],onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}
    listFilter (Right taskId) = {onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}

    read (_,items) = case [value \\ {TaskListItem|value} <- items] of
        [v:_]   = Ok v
        _       = Error (exception "taskListItemValue: item not found")

taskListItemProgress :: !(SharedTaskList a) -> SDSLens (Either Int TaskId) InstanceProgress () | TC a
taskListItemProgress tasklist = mapReadError read (toReadOnly (sdsTranslate "taskListItemProgress" listFilter tasklist))
where
    listFilter (Left index) = {onlyIndex=Just [index],onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=True}
    listFilter (Right taskId) = {onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=True}

    read (_,items) = case [p \\ {TaskListItem|progress=Just p} <- items] of
        [p:_]   = Ok p
        _       = Error (exception "taskListItemProgress: item not found")

mapMaybeLens :: !String !(sds () (Map a b) (Map a b)) -> SDSLens a (Maybe b) b | < a & == a & TC a & TC b & RWShared sds
mapMaybeLens name origShare = sdsLens name (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: !a !(Map a b) -> MaybeError TaskException (Maybe b) | < a & == a
  read idx m = Ok ('DM'.get idx m)

  write :: !a !(Map a b) !b -> MaybeError TaskException (Maybe (Map a b)) | < a & == a
  write idx oldmap newval = Ok (Just ('DM'.put idx newval oldmap))

  notify :: !a !(Map a b) !b -> SDSNotifyPred a | < a & == a
  notify idx oldmap newval = \idx` -> idx == idx`

mapLens :: !String !(sds () (Map a b) (Map a b)) !(Maybe b) -> SDSLens a b b | < a & == a & TC a & TC b & RWShared sds
mapLens name origShare mdef = sdsLens name (const ()) (SDSRead (read mdef)) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: !(Maybe b) !a !(Map a b) -> MaybeError TaskException b | < a & == a
  read mdef idx m
    = case 'DM'.get idx m of
        Just x -> Ok x
        _      -> case mdef of
                    Just def -> Ok def
                    _        -> Error (exception (name +++ " (mapLens): Index not found"))

  write :: !a !(Map a b) !b -> MaybeError TaskException (Maybe (Map a b)) | < a & == a
  write idx oldmap newval = Ok (Just ('DM'.put idx newval oldmap))

  notify :: !a !(Map a b) !b -> SDSNotifyPred a | < a & == a
  notify idx oldmap newval = \idx` -> idx == idx`

intMapLens :: !String !(sds () (IntMap a) (IntMap a)) !(Maybe a) -> SDSLens Int a a | TC a & RWShared sds
intMapLens name origShare mdef = sdsLens name (const ()) (SDSRead (read mdef)) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: !(Maybe a) !Int !(IntMap a) -> MaybeError TaskException a
  read mdef idx intmap
    = case 'DIS'.get idx intmap of
        Just x -> Ok x
        _      -> case mdef of
                    Just def -> Ok def
                    _        -> Error (exception (name +++ " (intMapLens): Index " +++ toString idx +++ " not found"))

  write :: !Int !(IntMap a) !a -> MaybeError TaskException (Maybe (IntMap a))
  write idx oldmap newval = Ok (Just ('DIS'.put idx newval oldmap))

  notify :: !Int !(IntMap a) !a -> SDSNotifyPred Int
  notify idx oldmap newval = \idx` -> idx == idx`
