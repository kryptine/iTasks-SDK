implementation module Collection

import iTasks
/*
* General purpose management task for a collection of data
*/
manageCollection :: !d !String (c -> i) (Shared [c]) -> Task (Maybe i) | descr d & iTask c & iTask i
manageCollection desc itemname identify collection
	= manageCollectionWith desc (selectItem ("Select " +++ itemname)) (viewItem ("Details of " +++ itemname))
		[AnyTime ActionNew (\_ -> addItem ("Add " +++ itemname) collection identify)
		,WithResult ActionEdit (const True) (editItem ("Edit " +++ itemname) collection (itemShare identify) identify)
		,WithResult ActionDelete (const True) (deleteItem ("Delete " +++ itemname,"Are you sure you want to delete the following " +++ itemname +++ "?") collection (itemShare identify) identify)
		]
		identify
		(itemShare identify)
		collection
/*
* Customizable management task for a collection of data
*/
manageCollectionWith ::
	!d																			//Description
	((Shared [c]) (ReadOnlyShared (Maybe i)) (c -> i) -> Task i)				//Make selection
	((Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task a)		//Use selection
	[TaskStep i (Maybe i)]														//Actions
	(c -> i)																	//Identification function
	((Shared [c]) i -> Shared (Maybe c))										//Item share function
	(Shared [c])																//Shared collection
	-> Task (Maybe i) | descr d & iTask c & iTask i & iTask a
manageCollectionWith desc makeSelection useSelection selectionActions identify itemShare collection
	= withShared Nothing (
		\selection ->
			parallel desc
				[//(Embedded, \_ -> (makeSelection collection (toReadOnly selection) identify) @> (\i _ -> Just i,selection)	@ Just)
			//	,(Embedded, \l -> forever ((watch selection @? onlyJust) >>* actions l)										@ const Nothing)
				/*,*/(Embedded, \_ -> forever (whileUnchanged selection (useSelection collection itemShare))					@ const Nothing)
				] @? firstJust
	)
where
	selShare l = mapRead toSel (taskListState l)
	where
		toSel [(Value _ v):_]	= Just v
		toSel _					= Nothing
		
	actions list = [inParallel step \\ step <- selectionActions]
	where
		inParallel = fixme where fixme = abort "Fix inParallel"
		/*
		inParallel (AnyTime action taskf)
			= AnyTime action (\mbi -> (appendTask Embedded (\_ -> (taskf mbi) <<@ Window)) list)
		inParallel (WithResult action pred taskf)
			= WithResult action (const True) (\i -> appendTask Embedded (\_ -> (taskf i) <<@ Window) list)
		*/
	
	onlyJust (Just (Just x))	= Just x
	onlyJust _					= Nothing
	
	firstJust (Value [(_,Value (Just x) s):_] _)	= Value x s
	firstJust _										= NoValue
import StdMisc

itemShare :: (c -> i) (Shared [c]) i -> Shared (Maybe c) | gEq{|*|} i & gEq{|*|} c
itemShare identify collection i = mapReadWrite (toItem,fromItem) collection
where
	toItem l	= case [c \\ c <- l | identify c === i] of
		[c]		= Just c
		_		= Nothing
	
	fromItem Nothing l 		= Just l
	fromItem (Just c`) l	= Just [if (identify c === i) c` c \\ c <- l]

selectItem :: !d (Shared [c]) (ReadOnlyShared (Maybe i)) (c -> i) -> Task i | descr d & iTask c & iTask i
selectItem desc collection selection identify
	=	enterSharedChoice desc [] collection
	@	identify

viewItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (Maybe i) -> Task (Maybe i) | descr d & iTask c & iTask i
viewItem desc collection itemShare Nothing	= viewInformation desc [] "Make a selection first..." @ const Nothing
viewItem desc collection itemShare (Just i)	= viewSharedInformation desc [] (itemShare collection i) @ const (Just i)

addItem :: !d (Shared [c]) (c -> i) -> Task (Maybe i) | descr d & iTask i & iTask c
addItem desc collection identify
	=	enterInformation desc []
	>>*	[AnyTime ActionCancel (\_ -> return Nothing)
		,WithResult ActionOk (const True) (\item -> update (\l -> l ++ [item]) collection >>| return (Just (identify item)))
		]

editItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | descr d & iTask c & iTask i
editItem desc collection itemShare identify i
	=	get (itemShare collection i)
	>>= \mbItem -> case mbItem of
			Nothing		= (return Nothing)
			(Just item)	=	updateInformation desc [] item
						>>*	[AnyTime ActionCancel (\_ -> return Nothing)
							,WithResult ActionOk (const True) (\item` -> 
													update (\l -> [if (identify c === i) item` c \\ c <- l ]) collection
													>>| return (Just i)
												 )
							]

deleteItem :: !d (Shared [c]) ((Shared [c]) i -> Shared (Maybe c)) (c -> i) i -> Task (Maybe i) | descr d & iTask c & iTask i
deleteItem desc collection itemShare identify i
	=	viewSharedInformation desc [] (itemShare collection i)
	>>*	[AnyTime ActionNo (\_ -> return Nothing)
		,AnyTime ActionYes (\_ -> update (\l -> [c \\ c <- l | identify c =!= i]) collection >>| return Nothing)
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

