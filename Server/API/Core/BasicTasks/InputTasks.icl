implementation module InputTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc, Functor
import Types, Shared, Util, TSt, ExceptionCombinators, CoreTasks, InteractionTasks, CoreCombinators, CommonCombinators
from StdFunc 		import id, const, o
from SharedTasks	import sharedStore, :: SharedStoreId
from SharedTasks	import qualified readShared, writeShared

//Local input
enterInformation :: !d -> Task a | descr d & iTask a
enterInformation d = enterInformation` d voidNothing
	
enterInformationA :: !d !(v -> a) ![PredAction (Verified a)] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask v
enterInformationA d view actions = enterInformationA` d view actions voidNothing
		
enterInformationAbout :: !d !about -> Task a | descr d & iTask a & iTask about
enterInformationAbout d about = enterInformation` d (Just about)
	
enterInformationAboutA :: !d !(v -> a) ![PredAction (Verified a)] !about -> Task (!Action,!Maybe a) | descr d  & iTask a & iTask about & iTask v
enterInformationAboutA d view actions about = enterInformationA` d view actions (Just about)

enterInformation` d mbAbout					= InputTask @>> interactLocal d (const (addAbout mbAbout [UpdateView (Unchanged Blank,id)])) okAction Nothing
enterInformationA` d view actions mbAbout	= InputTask @>> interactLocal d (const (addAbout mbAbout [UpdateView (Unchanged Blank,fmap view)])) (fromPredActionsLocal mb2Ver tuple actions) Nothing

//Shared input
enterSharedInformationA :: !d !(v -> w) ![PredAction (!Valid,!r)] !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w
enterSharedInformationA d view actions shared = enterSharedInformationA` d view actions shared voidNothing
	
enterSharedInformationAboutA :: !d !(v -> w) ![PredAction (!Valid,!r)] !about !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask about & iTask v & iTask w
enterSharedInformationAboutA d view actions about shared = enterSharedInformationA` d view actions shared (Just about)

enterSharedInformationA` d view actions shared mbAbout	= InputTask @>> interact d (\_ r _ -> addAbout mbAbout [UpdateView (Unchanged Blank,\mbV -> (isJust mbV,fmap view mbV))]) (fromPredActions (\a r _ -> (a,r)) (\a _ r _ -> (a,r)) actions) False shared

//Confirmation tasks
requestConfirmation	:: !d -> Task Bool | descr d
requestConfirmation d = requestConfirmation` d voidNothing
						
requestConfirmationAbout :: !d !about -> Task Bool | descr d & iTask about
requestConfirmationAbout d about = requestConfirmation` d (Just about)

requestConfirmation` d mbAbout = InputTask @>> interactLocal d (const (addAbout mbAbout [])) (const (UserActions [(ActionNo,Just False),(ActionYes,Just True)])) Void

//Local choice tasks
enterChoice :: !d ![a] -> Task a | descr d & iTask a
enterChoice _ [] = throw EmptyOptionList
enterChoice d options = enterChoice` d options voidNothing

enterChoiceA :: !d !(a -> v) ![PredAction (Verified a)] ![a] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask v
enterChoiceA _ _ _ [] = throw EmptyOptionList
enterChoiceA d view actions options = enterChoiceA` d view actions options voidNothing

enterChoiceAbout :: !d !about ![a] -> Task a | descr d & iTask a & iTask about
enterChoiceAbout _ _ [] = throw EmptyOptionList
enterChoiceAbout d about options = enterChoice` d options (Just about)

enterChoiceAboutA :: !d !(a -> v) ![PredAction (Verified a)] !about ![a] -> Task (!Action,!Maybe a) | descr d & iTask a & iTask about & iTask v
enterChoiceAboutA _ _ _ _ [] = throw EmptyOptionList
enterChoiceAboutA d view actions about options = enterChoiceA` d view actions options (Just about)

enterChoice` d options mbAbout					= InputTask @>> interactLocal d (\_ -> addAbout mbAbout [UpdateView (choiceFormView options,fmap getChoice)]) okAction Nothing
enterChoiceA` d view actions options mbAbout	= InputTask @>> interactLocal d (\_ -> addAbout mbAbout [UpdateView (choiceFormView (map view options),fmap getChoiceIndex)]) (fromPredActionsLocal (mb2Ver o fmap ((!!) options)) (\a mbIdx -> (a,fmap ((!!) options) mbIdx)) actions) Nothing
choiceFormView options = Unchanged (FormValue (choice options))

//Shared choice tasks
enterSharedChoiceA :: !d !(a -> v) ![PredAction (Verified a)] !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask v & iTask w
enterSharedChoiceA d view actions shared = enterSharedChoiceA` d view actions shared voidNothing

enterSharedChoiceAboutA :: !d !(a -> v) ![PredAction (Verified a)] !about !(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v & iTask w
enterSharedChoiceAboutA d view actions about shared = enterSharedChoiceA` d view actions shared (Just about)

enterSharedChoiceA` description view actions shared mbAbout
	= interact description interaction termination Nothing shared
where
	interaction local model changed
		= addAbout mbAbout [UpdateView (if changed initChoice (Unchanged initChoice),fromView)]
	where
		initChoice		= FormValue (choice (map view model))
		fromView mbC 	= (fmap getChoiceIndex mbC, Nothing)
	
	termination = fromPredActions (\l model _ -> mb2Ver (fmap ((!!) model) l)) (\action mbIdx model _ -> (action,fmap ((!!) model) mbIdx)) actions

//Local multiple choice tasks
enterMultipleChoice :: !d ![a] -> Task [a] | descr d & iTask a
enterMultipleChoice d options = enterMultipleChoice` d options voidNothing

enterMultipleChoiceA :: !d !(a -> v) ![PredAction [a]] ![a] -> Task (!Action,![a]) | descr d & iTask a & iTask v
enterMultipleChoiceA d view actions options = enterMultipleChoiceA` d view actions options voidNothing

enterMultipleChoiceAbout :: !d !about ![a] -> Task [a] | descr d & iTask a & iTask about
enterMultipleChoiceAbout d about options = enterMultipleChoice` d options (Just about)

enterMultipleChoiceAboutA :: !d !(a -> v) ![PredAction [a]] !about ![a] -> Task (!Action,![a]) | descr d & iTask a & iTask about & iTask v
enterMultipleChoiceAboutA d view actions about options = enterMultipleChoiceA` d view actions options (Just about)

enterMultipleChoice` d options mbAbout = InputTask @>> interactLocal d (\_ -> addAbout mbAbout [UpdateView (multipleChoiceFormView options,maybe [] getChoices)]) (\choice -> okAction (Just choice)) []
enterMultipleChoiceA` d view actions options mbAbout
	= InputTask @>> interactLocal
		d
		(\_ -> addAbout mbAbout [UpdateView (multipleChoiceFormView (map view options),\mbC -> maybe [] getChoiceIndexes mbC)])
		(fromPredActionsLocal (getIndexes options) (\a idxs -> (a,getIndexes options idxs)) actions)
		[]
multipleChoiceFormView options = Unchanged (FormValue (multipleChoice options))

//Shared multiple choice tasks
enterSharedMultipleChoiceA	:: !d !(a -> v) ![PredAction [a]] !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask v & iTask w
enterSharedMultipleChoiceA description view actions shared = enterSharedMultipleChoiceA` description view actions shared voidNothing

enterSharedMultipleChoiceAboutA	:: !d !(a -> v) ![PredAction [a]] !about !(Shared [a] w) -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v & iTask w
enterSharedMultipleChoiceAboutA description view actions about shared = enterSharedMultipleChoiceA` description view actions shared (Just about)

enterSharedMultipleChoiceA` description view actions shared mbAbout
	= interact description interaction termination [] shared
where
	interaction local model changed
		= addAbout mbAbout [UpdateView (toView,fromView)]
	where
		toView				= FormValue (multipleChoiceSel (map view model) (if changed [] local))
		fromView (Just mc)	= (getChoiceIndexes mc, Nothing)
		fromView Nothing	= (local, Nothing)
	
	termination local model _
		# choices = [a \\ a <- model & i <- [0..] |isMember i local] //Inefficient :(
		= UserActions [(action, if (pred choices) (Just (action,choices)) Nothing) \\ (action,pred) <- actions]