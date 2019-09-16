implementation module Incidone.Util.TaskPatterns

import iTasks, iTasks.Extensions.Dashboard
import iTasks.UI.Definition
import Incidone.OP.IncidentManagementTasks, Incidone.OP.ContactManagementTasks
import Text, Data.Functor, Data.Either, Data.Maybe
import qualified Data.Map as DM
import Data.Map.GenJSON
import StdMisc

//FIXME
/*
fillNotes :: [(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]
fillNotes cs = map fillNote cs
where
    fillNote (c=:(UIEditNote _ _),a) = (fillHeight c,'DM'.newMap)
    fillNote x = x
*/

/**
* Create a new incident containing no information at all.
*/
createNewIncident :: Task (Maybe IncidentNo)
createNewIncident
	=	Title "Create new incident" @>> Hint "Fill in the following basic information to create a new incident" @>> enterInformation []
	>>? createIncident

createNewContact :: Task (Maybe ContactNo)
createNewContact
	=	Title "New contact" @>> Hint "Enter the basic information of the new contact" @>> enterInformation []
	>>? createContact

indexedStore :: String v -> SDSLens k v v | Eq k & Ord k & iTask k & iTask v
indexedStore name def = sdsSplit "indexedStore" (\p -> ((),p)) read write (Just \p mapping. Ok (fromMaybe def ('DM'.get p mapping))) (sharedStore name 'DM'.newMap)
where
    read p mapping = fromMaybe def ('DM'.get p mapping)
    write p mapping v = ('DM'.put p v mapping,const ((==) p))

sdsDeref :: (sds1 p [a] [a]) (a -> Int) (sds2 [Int] [b] x) ([a] [b] -> [c]) -> (SDSSequence p [c] [a]) | iTask p & TC a & TC b & TC c & TC x & RWShared sds1 & RWShared sds2
sdsDeref sds1 toRef sds2 merge = sdsSequence "sdsDeref" paraml paramr (\_ _ -> Right read) writel writer sds1 sds2
where
	paraml p = p
	paramr p r1 = map toRef r1
    param _ r = (\_ r -> map toRef r)
    read (as,bs) = merge as bs

    writel = SDSWriteConst (\_ w -> Ok (Just w))
    writer = SDSWriteConst (\_ _ -> Ok Nothing)

viewDetails	:: (sds1 () (Maybe i) ()) (sds2 i c c) (c -> v) -> Task (Maybe v) | iTask i & iTask v & iTask c & RWShared sds1 & RWShared sds2
viewDetails sel target prj = viewSharedInformation [] (mapRead (fmap prj) (targetShare sel target))
where
	targetShare :: (sds1 () (Maybe i) ()) (sds2 i c c) -> SDSSequence () (Maybe c) () | iTask i & iTask c & RWShared sds1 & RWShared sds2
    targetShare sel target = sdsSequence "viewDetailsSeq" id (\_ i -> i) (\_ _ -> Right snd) writel writer sel (valueShare target)
    where
        writel = SDSWriteConst (\_ _ -> Ok Nothing)
        writer = SDSWriteConst (\_ _ -> Ok Nothing)

    valueShare :: (sds1 i c c) -> SDSSelect (Maybe i) (Maybe c) () | iTask i & iTask c & RWShared sds1
    valueShare target = sdsSelect "viewDetailsValue" param 
        (SDSNotifyConst (\_ _ _ _-> False)) 
        (SDSNotifyConst (\_ _ _ _-> False))
		(constShare Nothing) 
        (mapRead Just (toReadOnly target))
	where
    	param Nothing = Left ()
    	param (Just i) = Right i

optionalNewOrOpen   :: (String,Task ()) (String,i -> Task ()) Workspace (sds () (Maybe i) ()) -> Task () | iTask i & RWShared sds
optionalNewOrOpen (newLabel,newTask) (openLabel,openTask) ws selection
	= forever (
		watch selection >>*
			[OnAction (Action newLabel) (always (addToWorkspace (newTask <<@ InWindow) ws))
			,OnAction (Action openLabel) (ifValue isJust (\(Just c) -> addToWorkspace (doOrClose (openTask c)) ws))
			]
	)

doAddRemoveOpen     :: (Task a) (r -> Task b) (r -> Task c) Workspace (sds () (Maybe r) w) -> Task () | iTask a & iTask b & iTask c & iTask r & RWShared sds & TC w
doAddRemoveOpen  add remove open ws selection = forever
	(watch selection >>*
		[OnAction (Action "/Add")	  (always (addToWorkspace add ws))
		,OnAction (Action "/Remove")  (ifValue isJust	(\(Just sel) -> addToWorkspace (remove sel) ws))
		,OnAction (Action "/Open")    (ifValue isJust	(\(Just sel) -> addToWorkspace (open sel) ws))
		]
	)

//Move to util
viewAndEdit :: (Task a) (Task b) -> Task b | iTask a & iTask b
viewAndEdit view edit
    = forever (view >>* [OnAction (Action "Edit") (always edit)])

//Move to common tasks
viewOrEdit :: (Shared sds a) (a a -> Task ()) -> Task () | iTask a & RWShared sds
viewOrEdit s log
	= forever (view >>* [OnAction (Action "/Edit") (hasValue edit)]) @! ()
where
	view = viewSharedInformation [] s
	edit old
		=	updateInformation [] old
		>>?	\new ->
			set new s
        >>| log old new

doOrClose :: (Task a) -> Task (Maybe a) | iTask a
doOrClose task = ((task @ Just) -||- chooseAction [(ActionClose,Nothing)]) >>- return

doOrCancel :: (Task a) -> Task (Maybe a) | iTask a
doOrCancel task = (chooseAction [(ActionCancel,Nothing)] -||- (task @ Just)) >>- return

//withHeader :: (Task a) (Task b) -> Task b | iTask a	& iTask b
//withHeader headerTask bodyTask
//	= ((headerTask <<@ ForceLayout) ||- (bodyTask <<@ ForceLayout)) <<@ (ArrangeWithSideBar 0 TopSide 50 False)

withHeader :: (Task a) (Task b) -> Task b | iTask a	& iTask b
withHeader headerTask bodyTask
	= (headerTask ||- bodyTask ) //<<@ AfterLayout arrange
//FIXME
/*
where

    arrange ui=:{UIDef|content=UIBlocks [header,body] actions,windows}
        # (hcontrol,_,_,_) = blockToControl header
        # (UIContainer sOpts iOpts=:{UIItemsOpts|items},_,_,_) = blockToContainer body
        # bcontrol = case items of
            [item]  = UIContainer sOpts {UIItemsOpts|iOpts & items = [setHeight FlexSize item]}
            _       = UIContainer sOpts iOpts
        = {UIDef|content= UIBlock
            {UIBlock
            |attributes = header.UIBlock.attributes
            ,content = defaultItemsOpts [hcontrol,setHeight FlexSize bcontrol]
            ,actions = actions
            ,hotkeys = header.UIBlock.hotkeys ++ body.UIBlock.hotkeys
            ,size = defaultSizeOpts
            },windows = windows}
    arrange ui = ui
*/

viewNoSelection :: Task ()
viewNoSelection = viewTitle "Select..." @! ()

(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b
(>>?) taska taskbf = step taska (const Nothing)
                            [OnAction ActionCancel			(always (return Nothing))
							,OnAction ActionOk              (hasValue (\a -> taskbf a @ Just))
							,OnValue  					    (ifStable (\a -> taskbf a @ Just))
							]

oneOrAnother :: (String,Task a) (String,Task b) -> Task (Either a b) | iTask a & iTask b
oneOrAnother (labela,taska) (labelb,taskb)
    =   updateChoice [ChooseFromCheckGroup ((!!) [labela,labelb])]  [0,1] 0  <<@ ApplyLayout (setUIAttributes (heightAttr WrapSize))
    >&> \s -> whileUnchanged s (
        \choice -> case choice of
            Nothing = (viewInformation [] "You have to make a choice" @? const NoValue)
            (Just 0) = (taska @ Left)
            (Just 1) = (taskb @ Right)
        )

enterMultiple :: !String !Int (Task a) -> Task [a] | iTask a
enterMultiple action min task
    =   parallel ([(Embedded, const t) \\ t <- repeatn min task] ++ [(Embedded,more)]) [] @? res
where
    res (Value l _) = Value [v \\ (_,Value v _) <- l] (foldl allStable True l)

    allStable cur (_,Value _ s) = cur && s
    allStable cur _             = False

    more list =   viewInformation [] ()
              >>* [OnAction (Action action) (always (appendTask Embedded more list >>| task))]

manageSharedListWithDetails :: (Int -> Task ()) (Task Int) (Shared sds [Int]) -> Task () | RWShared sds
manageSharedListWithDetails detailsTask addTask refsList //Not the best implementation, but good enough for now
    =   get refsList
    >>- \initList ->
        parallel ([(Embedded, removeWhenStable (detailsTask i)) \\ i <- initList] ++ [(Embedded,add)]) []
    @! ()
where
    add list
        =   addTask
        >>- \i ->
            upd (\is -> is++[i]) refsList
        >>| appendTask Embedded add list
        >>| removeWhenStable (detailsTask i) list

    removeWhenStable t l = t >>* [OnValue (ifStable (\v -> get (taskListSelfId l) >>- \id -> removeTask id l @! v))]

manageBackgroundTask :: !String !String (Task a) -> Task () | iTask a
manageBackgroundTask identity title task
    =   viewSharedInformation [ViewAs (view title)] taskPid
    >^* [OnAction (Action "Start") (ifValue isNothing startTask)
        ,OnAction (Action "Stop") (ifValue isJust stopTask)
        ]
    @!  ()
where
    view title t = let (color,statusmsg) = status t in (color,title +++ " is " +++ statusmsg)
    status Nothing              = (LightOff,"not activated")
    status (Just (taskId,Unstable))  = (LightOnGreen,"running" <+++ taskId )
    status (Just (_,Stable))    = (LightOnGreen,"stopped")
    status (Just (_,Exception _)) = (LightOnRed,"stopped with an error")

    taskPid = mapRead find (sdsFocus ("name",JSONString identity) taskInstancesByAttribute)
    where
        find instances = case [(instanceNo,value) \\ {TaskInstance|instanceNo,value,attributes} <- instances | hasName identity attributes] of
            [(i,v):_]   = Just (TaskId i 0,v)
            _           = Nothing

        hasName name attributes = maybe False ((==) (JSONString name)) ('DM'.get "name" attributes)

    startTask _ = appendTask (Detached True) (removeWhenStable ((task <<@ ("name",JSONString identity)) @! ())) topLevelTasks @! ()
    stopTask (Just (taskId,_)) = removeTask taskId topLevelTasks @! ()

    removeWhenStable t l = t >>* [OnValue (ifStable (\_ -> get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue))]

syncNetworkChannel      :: String Int String (String -> m) (m -> String) (Shared sds ([m],Bool,[m],Bool)) -> Task () | iTask m & RWShared sds
syncNetworkChannel server port msgSeparator decodeFun encodeFun channel
    = tcpconnect server port Nothing channel {ConnectionHandlers|onConnect=onConnect,onData=onData,onShareChange=onShareChange,onDisconnect=onDisconnect,onDestroy= \s->(Ok s, [])} @! ()
where
    onConnect _ _ (received,receiveStopped,send,sendStopped)
        = (Ok "",if (not (isEmpty send)) (Just (received,False,[],sendStopped)) Nothing, map encodeFun send,False)

	onData newData acc (received,receiveStopped,send,sendStopped)
        # [acc:msgs]    = reverse (split msgSeparator (concat [acc,newData]))
		# write         = if (not (isEmpty msgs && isEmpty send))
            (Just (received ++ map decodeFun (reverse msgs),receiveStopped,[],sendStopped))
            Nothing
        = (Ok acc,write,map encodeFun send,False)

	onShareChange acc (received,receiveStopped,send,sendStopped)
		= (Ok acc,Nothing,[],False)

    onDisconnect l (received,receiveStopped,send,sendStopped)
		= (Ok l,Just (received,True,send,sendStopped))

consumeNetworkStream    :: ([m] -> Task ()) (Shared sds ([m],Bool,[m],Bool)) -> Task () | iTask m & RWShared sds
consumeNetworkStream processTask channel
    = ((watch channel >>* [OnValue (ifValue ifProcess process)]) <! id) @! ()
where
    ifProcess (received,receiveStopped,_,_)
        = receiveStopped || (not (isEmpty received))

    process (received,receiveStopped,_,_)
        =   upd empty channel
        >>| if (isEmpty received) (return ()) (processTask received)
        @!  receiveStopped

	empty :: ([m],Bool,[m],Bool) -> ([m],Bool,[m],Bool)
	empty (_,rs,s,ss) = ([],rs,s,ss)

