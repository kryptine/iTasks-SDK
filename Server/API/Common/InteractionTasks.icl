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
	mapToLocalState (updateSharedInformation` d options defaultValue voidNull)
where
	options		= filterOptions filterInputOptions defaultOpts options`
	defaultOpts	= [View (Nothing,Just (\l _ _ -> (Just l,Nothing)))]
	
updateInformation :: !d ![LocalViewOn m] m -> Task m | descr d & iTask m
updateInformation d options` m = UpdateTask @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options m voidNull)
where
	options								= filterOptions noFilter defaultOpts options`
	defaultOpts							= [View (Just (GetLocal id),Just (\l _ _ -> (Just l,Nothing)))]
	
showInformation :: !d ![LocalViewOn m] !m -> Task m | descr d & iTask m
showInformation d options` m = OutputTask PassiveOutput @>> LocalInteractionTask @>>
	mapToLocalState (updateSharedInformation` d options m voidNull)
where
	options		= filterOptions filterOutputOptions defaultOpts options`
	defaultOpts	= [View (Just (GetLocal id),Nothing)]
	
mapToLocalState = mapActionTaskModelValue (\(l,_) -> l)

enterSharedInformation :: !d ![ViewOn l r w] !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
enterSharedInformation d options` shared = InputTask @>> updateSharedInformation` d options defaultValue shared
where
	options		= filterOptions filterInputOptions defaultOpts options`
	defaultOpts	= [View (Nothing,Just (\l _ _ -> (Just l,Nothing))), View (Nothing,Just (\w _ _ -> (Nothing,Just w)))]
	
updateSharedInformation :: !d ![ViewOn l r w] l !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
updateSharedInformation d options` initLocal shared = UpdateTask @>> updateSharedInformation` d options initLocal shared
where
	options								= filterOptions noFilter defaultOpts options`
	defaultOpts							= [defaultLocalView,defaultSharedView]
	defaultLocalView					= View (Just (GetLocal id),Just (\l _ _ -> (Just l,Nothing)))
	defaultSharedView = case dynamic id :: A.a: (a -> a) of
		(putback :: (r^ -> w^))			= View (Just (GetShared id),Just (\r _ _ -> (Nothing,Just (putback r))))
		_								= View (Just (GetShared id),Nothing)

showSharedInformation :: !d ![ViewOn l r w] !l !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
showSharedInformation d options` local shared = OutputTask PassiveOutput @>> updateSharedInformation` d options local shared
where
	options		= filterOptions filterOutputOptions defaultOpts options`
	defaultOpts	= [View (Just (GetLocal id),Nothing), View (Just (GetShared id),Nothing)]

updateSharedInformation` :: !d ![ViewOn l r w] l !(ReadWriteShared r w) -> Task (l,r) | descr d & iTask l & iTask r & iTask w
updateSharedInformation` d options initLocal shared = mapActionTaskModelValue (\((l,_,_),r) -> (l,r)) (interact d interaction (initLocal,False,False) shared)
where
	interaction (l,updateLocalViews,updateSharedViews) r changed = catMaybes (map mkPart options)
	where
		mkPart part = case part of
			(About a)						= Just (DisplayView a)
			(View (Nothing,Just putback))	= Just (UpdateView
												(Unchanged Blank)
												(mkPutback putback l r))
			(View (Just get,Nothing)) = case get of
				GetLocal get				= Just (DisplayView (get l))
				GetShared get				= Just (DisplayView (get r))
				GetLocalAndShared get		= Just (DisplayView (get l r))
			(View (Just get,Just putback))
				# updateViewGet = case get of
					GetLocal get			= let form = FormValue (get l) in if updateLocalViews										form (Unchanged form)
					GetShared get			= let form = FormValue (get r) in if (updateSharedViews || changed)							form (Unchanged form)
					GetLocalAndShared get	= let form = FormValue (get l r) in if (updateLocalViews || updateSharedViews || changed)	form (Unchanged form)
				= Just (UpdateView
					updateViewGet
					(mkPutback putback l r))
			_								= Nothing
									
		mkPutback putback l r mbV = case mbV of
			Nothing = ((l,False,False),Nothing)
			Just v
				# (mbL,mbW)			= putback v l r
				# updateSharedViews	= isJust mbW
				= (maybe (l,False,updateSharedViews) (\l -> (l,True,updateSharedViews)) mbL,mbW)
	
filterOptions filterF defaultOpts options = addDefault (catMaybes (map filterF options))
where	
	addDefault options
		| any (\o -> case o of (About _) = False; _ = True) options	= options
		| otherwise													= options ++ defaultOpts
		
filterInputOptions option = case option of
		About a					= Just (About a)
		View (_,Just putback)	= Just (View (Nothing,Just putback))
		_						= Nothing
		
filterOutputOptions option = case option of
		About a				= Just (About a)
		View (Just get,_)	= Just (View (Just get,Nothing))
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
	interaction mbLocal choiceOpts _ = addAbouts options [UpdateView toView fromView]
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
	interaction mbLocal choiceOpts _	= addAbouts options [UpdateView toView fromView]
	where
		toView							= FormValue (setOptionsM (toViewOptions options choiceOpts) (fromMaybe (initMultipleChoice choiceOpts) mbLocal))
		fromView mc						= (mc, Nothing)
	initMultipleChoice choiceOpts		= multipleChoiceSel (toViewOptions options choiceOpts) (toViewOptions options sel)

addAbouts options parts = [DisplayView a \\ About a <- options] ++ parts

toSharedChoiceViews :: ![LocalViewOn o] -> [ViewOn Void o w]
toSharedChoiceViews options = catMaybes (map toSharedChoiceView options)
where
	toSharedChoiceView opt = case opt of
		About a							= Just (About a)
		View (Just get,putback)
			# get` = case get of
				GetLocal get			= GetShared get
				GetLocalAndShared get	= GetLocalAndShared (flip get)
				GetShared get			= GetLocal get
			= Just (View (Just get`,Nothing))
		_								= Nothing

// special type for choice options of type a, providing a different view already visualized as string
:: ChoiceOption a = ChoiceOption !String !a

toViewOption options o				= hd [ChoiceOption (visualizeAsTextLabel (get o)) o \\ View (Just (GetShared get),Nothing) <- filterOptions filterOutputOptions [View (Just (GetShared id),Nothing)] options]
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
	(showSharedInformation ("Wait for time", ("Wait until " +++ toString time)) [] Void currentTime >? \(_,now) -> time < now) >>= transform snd

waitForDate :: !Date -> Task Date
waitForDate date =
	(showSharedInformation ("Wait for date", ("Wait until " +++ toString date)) [] Void currentDate >? \(_,now) -> date < now) >>= transform snd

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

	toSharedRes (UpdateView formView putback)	= UpdateView formView (\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
voidNull :: Shared Void
voidNull = null

constShared a = makeReadOnlyShared ("const_" +++ toString (toJSON a)) (\world -> (a,world)) (\world -> (Timestamp 0,world))