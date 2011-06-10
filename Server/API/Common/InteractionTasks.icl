implementation module InteractionTasks

from StdFunc import id, const, o, flip
from SystemData import null
from Util import appSnd
from Shared import makeReadOnlyShared, :: SharedId
from Time import :: Timestamp(..)
import StdBool, StdList, StdMisc, StdTuple
import CoreTasks, TuningCombinators, CoreCombinators, ExceptionCombinators, SystemData, CommonCombinators

enterInformation :: !d ![LocalViewOn m] -> Task m | descr d & iTask m
enterInformation d options` = InputTask @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options voidNull defaultValue)
where
	options		= filterOptions filterInputOptions defaultOpts options`
	defaultOpts	= [EnterView (PutbackLocal (\l _ _ -> l))]
	
updateInformation :: !d ![LocalViewOn m] m -> Task m | descr d & iTask m
updateInformation d options` m = UpdateTask @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options voidNull m)
where
	options		= filterOptions noFilter defaultOpts options`
	defaultOpts	= [UpdateView (GetLocal id, PutbackLocal (\l _ _ -> l))]
	
showInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m
showInformation d options` m = OutputTask PassiveOutput @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options voidNull m)
where
	options		= filterOptions filterOutputOptions defaultOpts options`
	defaultOpts	= [ShowView (GetLocal id)]
	
mapToLocalState = mapActionTaskModelValue (\(_,l) -> l)

enterSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) -> Task (r,l) | descr d & iTask l & iTask r & iTask w
enterSharedInformation d options` shared = InputTask @>> updateSharedInformation` d options shared defaultValue
where
	options		= filterOptions filterInputOptions defaultOpts options`
	defaultOpts	= [EnterView (PutbackLocal (\l _ _ -> l)), EnterView (PutbackShared (\w _ _ -> w))]
	
updateSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) l -> Task (r,l) | descr d & iTask l & iTask r & iTask w
updateSharedInformation d options` shared initLocal = UpdateTask @>> updateSharedInformation` d options shared initLocal
where
	options								= filterOptions noFilter defaultOpts options`
	defaultOpts							= [defaultLocalView,defaultSharedView]
	defaultLocalView					= UpdateView (GetLocal id, PutbackLocal (\l _ _ -> l))
	defaultSharedView = case dynamic id :: A.a: (a -> a) of
		(putback :: (r^ -> w^))			= UpdateView	(GetShared id,PutbackShared (\r _ _ -> (putback r)))
		_								= ShowView		(GetShared id)

showSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) !l -> Task (r,l) | descr d & iTask l & iTask r & iTask w
showSharedInformation d options` shared local = OutputTask PassiveOutput @>> updateSharedInformation` d options shared local
where
	options		= filterOptions filterOutputOptions defaultOpts options`
	defaultOpts	= [ShowView (GetLocal id), ShowView (GetShared id)]

updateSharedInformation` :: !d ![ViewOn l r w] !(ReadWriteShared r w)  l -> Task (r,l) | descr d & iTask l & iTask r & iTask w
updateSharedInformation` d options shared initLocal = mapActionTaskModelValue (\((l,_,_),r) -> (r,l)) (interact d interaction (initLocal,False,False) shared)
where
	interaction (l,updateLocalViews,updateSharedViews) r changed = map mkPart options
	where
		mkPart part = case part of
			About a							= DisplayPart a
			EnterView putback				= FormPart	(Unchanged Blank)
														(mkPutback putback l r)
			ShowView get = case get of
				GetLocal get				= DisplayPart (get l)
				GetShared get				= DisplayPart (get r)
				GetLocalAndShared get		= DisplayPart (get l r)
			UpdateView (get,putback)
				# updateViewGet = case get of
					GetLocal get			= let form = FormValue (get l) in if updateLocalViews										form (Unchanged form)
					GetShared get			= let form = FormValue (get r) in if (updateSharedViews || changed)							form (Unchanged form)
					GetLocalAndShared get	= let form = FormValue (get l r) in if (updateLocalViews || updateSharedViews || changed)	form (Unchanged form)
				= FormPart
					updateViewGet
					(mkPutback putback l r)
									
		mkPutback putback l r mbV = case mbV of
			Nothing = ((l,False,False),Nothing)
			Just v = case putback of
				Putback putback
					# (mbL,mbW)			= putback v l r
					# updateSharedViews	= isJust mbW
					= (maybe (l,False,updateSharedViews) (\l -> (l,True,updateSharedViews)) mbL,mbW)
				PutbackLocal putback	= ((putback v l r, True, False), Nothing)
				PutbackShared putback	= ((l, False, True), Just (putback v l r))
	
filterOptions filterF defaultOpts options = addDefault (catMaybes (map filterF options))
where	
	addDefault options
		| any (\o -> case o of (About _) = False; _ = True) options	= options
		| otherwise													= options ++ defaultOpts
		
filterInputOptions option = case option of
		About a				= Just (About a)
		EnterView e			= Just (EnterView e)
		UpdateView (_,e)	= Just (EnterView e)
		_					= Nothing
		
filterOutputOptions option = case option of
		About a				= Just (About a)
		ShowView s			= Just (ShowView s)
		UpdateView (s,_)	= Just (ShowView s)
		_					= Nothing
		
noFilter = Just

enterChoice :: !d ![LocalViewOn o] ![o] -> Task o | descr d & iTask o
enterChoice d options choiceOpts = InputTask @>> LocalInteractionTask @>>
	choice` d (toSharedChoiceViews options) (constShared choiceOpts) Nothing

updateChoice :: !d ![LocalViewOn o] ![o] o -> Task o | descr d & iTask o
updateChoice d options choiceOpts initC = UpdateTask @>> LocalInteractionTask @>>
	choice` d (toSharedChoiceViews options) (constShared choiceOpts) (Just initC)
	
enterSharedChoice :: !d ![ViewOn Void o w] !(ReadWriteShared [o] w) -> Task o | descr d & iTask o & iTask w
enterSharedChoice d options shared = InputTask @>> choice` d options shared Nothing

updateSharedChoice :: !d ![ViewOn Void o w] !(ReadWriteShared [o] w) o -> Task o | descr d & iTask o & iTask w
updateSharedChoice d options shared initC = UpdateTask @>> choice` d options shared (Just initC)

choice` d options shared mbSel =
	mapActionTaskModelValue (\(mbC,choiceOpts) -> maybe defaultValue (\c -> maybe defaultValue fromViewOption (getMbChoice c)) (fmap (setOptions (toViewOptions options choiceOpts)) mbC)) (interact d interaction Nothing shared)
where
	interaction mbLocal choiceOpts _ = addAbouts options [FormPart toView fromView]
	where
		toView				= FormValue (setOptions (viewOptions choiceOpts) (fromMaybe (initChoice choiceOpts) mbLocal))
		fromView c			= (c, Nothing)
	initChoice choiceOpts	= maybe (choice (viewOptions choiceOpts)) (\sel -> choiceSel (viewOptions choiceOpts) (toViewOption options sel)) mbSel
	viewOptions choiceOpts	= toViewOptions options choiceOpts

enterMultipleChoice :: !d ![LocalViewOn o] ![o] -> Task [o] | descr d & iTask o
enterMultipleChoice d options choiceOpts = InputTask @>> LocalInteractionTask @>>
	multipleChoice` d (toSharedChoiceViews options) (constShared choiceOpts) []

updateMultipleChoice :: !d ![LocalViewOn o] ![o] [o] -> Task [o] | descr d & iTask o
updateMultipleChoice d options choiceOpts initC = UpdateTask @>> LocalInteractionTask @>>
	multipleChoice` d (toSharedChoiceViews options) (constShared choiceOpts) initC

enterSharedMultipleChoice :: !d ![ViewOn Void o w] !(ReadWriteShared [o] w) -> Task [o] | descr d & iTask o & iTask w
enterSharedMultipleChoice d options shared = InputTask @>> multipleChoice` d options shared []

updateSharedMultipleChoice :: !d ![ViewOn Void o w] !(ReadWriteShared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w
updateSharedMultipleChoice d options shared sel = UpdateTask @>> multipleChoice` d options shared sel

multipleChoice` d options shared sel =
	mapActionTaskModelValue (\(mbLocal,choiceOpts) -> fromViewOptions (maybe [] getChoices (fmap (setOptionsM (toViewOptions options choiceOpts)) mbLocal))) (interact d interaction Nothing shared)
where	
	interaction mbLocal choiceOpts _	= addAbouts options [FormPart toView fromView]
	where
		toView							= FormValue (setOptionsM (toViewOptions options choiceOpts) (fromMaybe (initMultipleChoice choiceOpts) mbLocal))
		fromView mc						= (mc, Nothing)
	initMultipleChoice choiceOpts		= multipleChoiceSel (toViewOptions options choiceOpts) (toViewOptions options sel)

addAbouts options parts = [DisplayPart a \\ About a <- options] ++ parts

toSharedChoiceViews :: ![LocalViewOn o] -> [ViewOn Void o w]
toSharedChoiceViews options = catMaybes (map toSharedChoiceView options)
where
	toSharedChoiceView opt = case opt of
		About a							= Just (About a)
		ShowView get
			# get` = case get of
				GetLocal get			= GetShared get
				GetLocalAndShared get	= GetLocalAndShared (flip get)
				GetShared get			= GetLocal get
			= Just (ShowView get`)
		_								= Nothing

// special type for choice options of type a, providing a different view already visualized as string
:: ChoiceOption a = ChoiceOption !String !a

toViewOption options o = case [ChoiceOption (visualizeAsTextLabel (get o)) o \\ ShowView (GetShared get) <- options] of
	[opt:_]	= opt
	_		= ChoiceOption (visualizeAsTextLabel o) o
fromViewOption (ChoiceOption _ o)	= o
toViewOptions options				= map (toViewOption options)
fromViewOptions						= map fromViewOption

derive gUpdate		ChoiceOption
derive gDefaultMask	ChoiceOption
derive gVerify		ChoiceOption
derive JSONEncode	ChoiceOption
derive JSONDecode	ChoiceOption
gEq{|ChoiceOption|} f (ChoiceOption _ x) (ChoiceOption _ y) = f x y
gVisualize{|ChoiceOption|} _ mbV vst = case mbV of
	Just (ChoiceOption label _)	= ([TextFragment label],vst)
	Nothing						= ([],vst)

waitForTime :: !Time -> Task Time
waitForTime time =
	(showSharedInformation ("Wait for time", ("Wait until " +++ toString time)) [] currentTime Void >? \(now,_) -> time < now) >>= transform fst

waitForDate :: !Date -> Task Date
waitForDate date =
	(showSharedInformation ("Wait for date", ("Wait until " +++ toString date)) [] currentDate Void >? \(now,_) -> date < now) >>= transform fst

waitForTimer :: !Time -> Task Time
waitForTimer time = get currentTime >>= \now -> waitForTime (now + time)

chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions = interactLocal chooseActionDescr (const []) Void >>+ \_ -> UserActions (map (appSnd Just) actions)

chooseActionDyn :: !(ReadWriteShared r w) !(r -> [(!Action,Maybe a)]) -> Task a | iTask a & iTask r & iTask w
chooseActionDyn shared actionsF = interact chooseActionDescr (\_ _ _ -> []) Void shared >>+ \{modelValue=v=:(_,r)} -> UserActions (actionsF r)

chooseActionDescr = "Choose an action"

interactLocal :: !d !(l -> [InteractionPart l]) l -> Task l | descr d & iTask l
interactLocal d partFunc l = LocalInteractionTask @>> mapActionTask (\st=:{modelValue=v=:(l,_)} -> {st & modelValue = l}) (interact d interaction l voidNull)
where
	interaction l _ _ = map toSharedRes (partFunc l)

	toSharedRes (FormPart formView putback)	= FormPart formView (\mbV -> (putback mbV,Nothing))
	toSharedRes (UpdatePart label l)				= UpdatePart label (l,Nothing)
	toSharedRes (DisplayPart v)						= DisplayPart v
	
voidNull :: Shared Void
voidNull = null

constShared a = makeReadOnlyShared ("const_" +++ toString (toJSON a)) (\world -> (a,world)) (\world -> (Timestamp 0,world))