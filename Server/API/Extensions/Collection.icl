implementation module Collection

import iTasks

/*
* General purpose management task for a collection of data
*/
manageCollection :: !d !String (c -> i) (Shared [c]) -> Task (Maybe i) | descr d & iTask c & iTask i
manageCollection desc itemname identify collection
	= manageCollectionWith desc (selectItem ("Select " +++ itemname)) (viewItem ("Details of " +++ itemname))
		[(ActionNew, Always (addItem ("Add " +++ itemname) collection identify))
		,(ActionEdit, IfValid (editItem ("Edit " +++ itemname) collection (itemShare identify) identify))
		,(ActionDelete,IfValid (deleteItem ("Delete " +++ itemname,"Are you sure you want to delete the following " +++ itemname +++ "?") collection (itemShare identify) identify))
		]
		identify
		(itemShare identify)
		collection
/*
* Customizable management task for a collection of data
*/
manageCollectionWith ::
	!d																			//Description
	((Shared [c]) (Shared (Maybe i)) (c -> i) -> Task (Maybe i))				//Make selection
	((Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task a)		//Use selection
	[(!Action,!TaskContinuation i (Maybe i))]									//Actions
	(c -> i)																	//Identification function
	((Shared [c]) i -> Shared (Maybe c))										//Item share function
	(Shared [c])																//Shared collection
	-> Task (Maybe i) | descr d & iTask c & iTask i & iTask a
manageCollectionWith desc makeSelection useSelection selectionActions identify itemShare collection
	= parallel desc Nothing (\_ s -> s)
		[(Embedded, \l -> forever (makeSelection collection (taskListState l) identify >?* (actions l)))
		,(Embedded, \l -> forever (whileUnchanged (taskListState l) (useSelection collection itemShare )))
		]	
where
	actions list = [(action,inParallel continuation) \\ (action,continuation) <- selectionActions]
	where
		inParallel (Always task)	= Always (appendTask (Embedded, \_ -> (task >>| return Continue) <<@ Window) list )
		inParallel (IfValid taskf)	= IfHolds isJust (\(Just i) -> appendTask (Embedded, \_ -> (taskf i >>| return Continue) <<@ Window) list)

itemShare :: (c -> i) (Shared [c]) i -> Shared (Maybe c) | gEq{|*|} i & gEq{|*|} c
itemShare identify collection i = mapShared (toItem,fromItem) collection
where
	toItem l	= case [c \\ c <- l | identify c === i] of
		[c]		= Just c
		_		= Nothing
	
	fromItem Nothing l 		= l
	fromItem (Just c`) l	= [if (identify c === i) c` c \\ c <- l]

selectItem :: !d (Shared [c]) (Shared (Maybe i)) (c -> i) -> Task (Maybe i) | descr d & iTask c & iTask i
selectItem desc collection selection identify
	=	narrowDown desc identify collection selection
	
viewItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task (Maybe i) | descr d & iTask c & iTask i
viewItem desc collection itemShare Nothing	= viewInformation desc [] "Make a selection first..." >>$ const Nothing
viewItem desc collection itemShare (Just i)	= viewSharedInformation desc [] (itemShare collection i) >>$ const (Just i)

addItem :: !d (Shared [c]) (c -> i) -> Task (Maybe i) | descr d & iTask i & iTask c
addItem desc collection identify
	=	enterInformation desc []
	>?*	[(ActionCancel, Always (return Nothing))
		,(ActionOk, IfValid (\item -> update (\l -> l ++ [item]) collection >>| return (Just (identify item))))
		]

editItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | descr d & iTask c & iTask i
editItem desc collection itemShare identify i
	=	get (itemShare collection i)
	>>= \mbItem -> case mbItem of
			Nothing		= (return Nothing)
			(Just item)	=	updateInformation desc [] item
						>?*	[(ActionCancel, Always (return Nothing))
							,(ActionOk, IfValid (\item` -> 
													update (\l -> [if (identify c === i) item` c \\ c <- l ]) collection
													>>| return (Just i)
												 ))
							]

deleteItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | descr d & iTask c & iTask i
deleteItem desc collection itemShare identify i
	=	viewSharedInformation desc [] (itemShare collection i)
	>?*	[(ActionNo, Always (return Nothing))
		,(ActionYes, Always (update (\l -> [c \\ c <- l | identify c =!= i]) collection >>| return Nothing))
		]

narrowDown :: !d (a -> b) (Shared [a]) (Shared (Maybe b)) -> Task (Maybe b) | descr d & iTask a & iTask b
narrowDown desc f options selection =
	updateSharedInformation desc [UpdateView (GetShared toView) fromView] (options |+< selection)
where
	//Use a grid choice type to indicate the choice to make the 
	toView (opts,sel)	= GridChoice [(a,a) \\ a <- opts] (selIndex 0 opts sel)

	//Write the selection to the shared
	fromView choice _ _ = fmap f (getMbSelection choice)

	selIndex i _ Nothing	= Nothing
	selIndex i [] _			= Nothing
	selIndex i [o:os] (Just sel) = if (f o === sel) (Just i) (selIndex (inc i) os (Just sel))

