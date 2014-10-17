implementation module Incidone.Util.TaskPatterns

import iTasks, iTasks.API.Extensions.Dashboard
import Incidone.OP.IncidentManagementTasks, Incidone.OP.ContactManagementTasks
import Text, Data.Functor, Data.Either
import qualified Data.Map as DM

NoAnnotation	:== AfterLayout (tweakControls (map (\(c,_) -> (c,'DM'.newMap))))

fillNotes :: [(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]
fillNotes cs = map fillNote cs
where
    fillNote (c=:(UIEditNote _ _),a) = (fillHeight c,'DM'.newMap)
    fillNote x = x

/**
* Create a new incident containing no information at all.
*/
createNewIncident :: Task (Maybe IncidentNo)
createNewIncident
	=	enterInformation ("Create new incident", "Fill in the following basic information to create a new incident") []
	>>? createIncident
	
createNewContact :: Task (Maybe ContactNo)
createNewContact
	=	enterInformation ("New contact","Enter the basic information of the new contact") []
	>>? createContact

indexedStore :: String v -> RWShared k v v | Eq k & Ord k & iTask k & iTask v
indexedStore name def = sdsSplit "indexedStore" (\p -> ((),p)) read write (sharedStore name 'DM'.newMap)
where
    read p mapping = fromMaybe def ('DM'.get p mapping)
    write p mapping v = ('DM'.put p v mapping,(==) p)

sdsDeref :: (RWShared p [a] [a]) (a -> Int) (RWShared [Int] [b] x) ([a] [b] -> [c]) -> (RWShared p [c] [a])
sdsDeref sds1 toRef sds2 merge = sdsSequence "sdsDeref" (\_ r -> map toRef r) read writel writer sds1 sds2
where
    param _ r = (\_ r -> map toRef r)
    read (as,bs) = merge as bs
    writel = SDSWriteConst (\_ w -> Ok (Just w))
    writer = SDSWriteConst (\_ _ -> Ok Nothing)

viewDetails	:: !d (ReadOnlyShared (Maybe i)) (RWShared i c c) (c -> v) -> Task (Maybe v) | descr d & iTask i & iTask v
viewDetails desc sel target prj = viewSharedInformation desc [] (mapRead (fmap prj) targetShare)
where
    targetShare = sdsSequence "viewDetailsSeq" (\_ i -> i) snd writel writer sel valueShare
    where
        writel = SDSWriteConst (\_ _ -> Ok Nothing)
        writer = SDSWriteConst (\_ _ -> Ok Nothing)
    valueShare = sdsSelect "viewDetailsValue" param (\_ _ _ _ -> False) (\_ _ _ _ -> False) (constShare Nothing) (mapRead Just (toReadOnly target))

    param Nothing = Left ()
    param (Just i) = Right i

optionalNewOrOpen :: (String,Task ()) (String,i -> Task ()) Workspace (ReadOnlyShared (Maybe i)) -> Task () | iTask i
optionalNewOrOpen (newLabel,newTask) (openLabel,openTask) ws selection
	= forever (
		watch selection >>*
			[OnAction (Action newLabel [ActionIcon "add"]) (always (addToWorkspace (newTask <<@ InWindow) ws))
			,OnAction (Action openLabel [ActionIcon "open"]) (ifValue isJust (\(Just c) -> addToWorkspace (doOrClose (openTask c)) ws))
			]
	)

doAddRemoveOpen :: (Task a) (r -> Task b) (r -> Task c) Workspace (ReadWriteShared (Maybe r) w) -> Task () | iTask a & iTask b & iTask c & iTask r
doAddRemoveOpen  add remove open ws selection = forever
	(watch selection >>*
		[OnAction (Action "/Add" [ActionIcon "add"])	    (always (addToWorkspace add ws))
		,OnAction (Action "/Remove" [ActionIcon "remove"])  (ifValue isJust	(\(Just sel) -> addToWorkspace (remove sel) ws))
		,OnAction (Action "/Open" [ActionIcon "open"])      (ifValue isJust	(\(Just sel) -> addToWorkspace (open sel) ws))
		]
	)

//Move to util
viewAndEdit :: (Task a) (Task b) -> Task b | iTask a & iTask b
viewAndEdit view edit
    = forever (view >>* [OnAction (Action "Edit" [ActionIcon "edit"]) (always edit)])

//Move to common tasks
viewOrEdit :: d (Shared a) (a a -> Task ()) -> Task () | descr d & iTask a
viewOrEdit prompt s log
	= forever (view >>* [OnAction (Action "/Edit" [ActionIcon "edit"]) (hasValue edit)]) @! ()
where
	view = viewSharedInformation prompt [] s
	edit old
		=	updateInformation prompt [] old
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
	= ((headerTask <<@ ForceLayout) ||- (bodyTask <<@ ForceLayout)) <<@ AfterLayout arrange
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

viewNoSelection :: Task ()
viewNoSelection = viewTitle "Select..." @! ()

(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b
(>>?) taska taskbf = step taska (const Nothing)
                            [OnAction ActionCancel			(always (return Nothing))
							,OnAction ActionOk              (hasValue (\a -> taskbf a @ Just))
							,OnValue  					    (ifStable (\a -> taskbf a @ Just))
							]

oneOrAnother :: !d (String,Task a) (String,Task b) -> Task (Either a b) | descr d & iTask a & iTask b
oneOrAnother desc (labela,taska) (labelb,taskb)
    =   updateChoice desc [ChooseWith (ChooseFromRadioButtons ((!!) [labela,labelb]))]  [0,1] 0 <<@ AfterLayout (uiDefSetHeight WrapSize)
    >&> \s -> whileUnchanged s (
        \choice -> case choice of
            Nothing = (viewInformation () [] "You have to make a choice" @? const NoValue)
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

    more list =   viewInformation () [] ()
              >>* [OnAction (Action action [ActionIcon "add"]) (always (appendTask Embedded more list >>| task))]

manageSharedListWithDetails :: (Int -> Task ()) (Task Int) (Shared [Int]) -> Task ()
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

manageBackgroundTask :: !d !String !String (Task a) -> Task () | descr d & iTask a
manageBackgroundTask d identity title task
    =   viewSharedInformation d [ViewWith (view title)] taskPid
    >^* [OnAction (Action "Start" []) (ifValue isNothing startTask)
        ,OnAction (Action "Stop" []) (ifValue isJust stopTask)
        ]
    @!  ()
where
    view title t = let (color,statusmsg) = status t in Row (color,title +++ " is " +++ statusmsg)
    status Nothing              = (LightOff,"not activated")
    status (Just (taskId,None))      = (LightOnGreen,"running " <+++ taskId )
    status (Just (taskId,Unstable))  = (LightOnGreen,"running" <+++ taskId )
    status (Just (_,Stable))    = (LightOnGreen,"stopped")
    status (Just (_,Exception)) = (LightOnRed,"stopped with an error")

    taskPid = mapRead find allTaskInstances //TODO: Use a filter on attributes
    where
        find instances = case [(instanceNo,value) \\ {TaskInstance|instanceNo,value,attributes} <- instances | hasName identity attributes] of
            [(i,v):_]   = Just (TaskId i 0,v)
            _           = Nothing

        hasName name attributes = maybe False ((==) name) ('DM'.get "name" attributes)

    startTask _ = appendTask (NamedDetached identity defaultValue True) (removeWhenStable (task @! ())) topLevelTasks @! ()
    stopTask (Just (taskId,_)) = removeTask taskId topLevelTasks @! ()

    removeWhenStable t l = t >>* [OnValue (ifStable (\_ -> get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue))]

syncNetworkChannel :: String Int String (String -> m) (m -> String) (Shared ([m],Bool,[m],Bool)) -> Task () | iTask m
syncNetworkChannel server port msgSeparator decodeFun encodeFun channel
    = tcpconnect server port channel onConnect onData @! ()
where
    onConnect (received,receiveStopped,send,sendStopped)
        = (Ok "",if (not (isEmpty send)) (Just (received,False,[],sendStopped)) Nothing, map encodeFun send,False)
    onData acc (received,receiveStopped,send,sendStopped) newData channelChanged connectionClosed
        # [acc:msgs]    = reverse (split msgSeparator (concat [acc:newData]))
        # write         = if (not (isEmpty msgs && isEmpty send) || connectionClosed)
            (Just (received ++ map decodeFun (reverse msgs),connectionClosed,[],sendStopped))
            Nothing
        = (Ok acc,write,map encodeFun send,False)

consumeNetworkStream :: ([m] -> Task ()) (Shared ([m],Bool,[m],Bool)) -> Task () | iTask m
consumeNetworkStream processTask channel
    = ((watch channel >>* [OnValue (ifValue ifProcess process)]) <! id) @! ()
where
    ifProcess (received,receiveStopped,_,_)
        = receiveStopped || (not (isEmpty received))

    process (received,receiveStopped,_,_)
        =   upd (\(_,rs,s,ss) -> ([],rs,s,ss)) channel
        >>| if (isEmpty received) (return ()) (processTask received)
        @!  receiveStopped
