implementation module iTasks.API.Common.InteractionTasks

from StdFunc import id, const, o, flip
from iTasks.API.Core.SDSs import null
from Data.Tuple import appSnd
from Data.List import isMemberGen, findIndex, instance Functor []
from System.Time import :: Timestamp(..)
from Data.Map import qualified get, put

import StdBool, StdList, StdMisc, StdTuple, Data.Functor
import iTasks.API.Core.Tasks, iTasks.API.Core.TaskCombinators
import iTasks.API.Common.TaskCombinators, iTasks.API.Core.SDSs
import iTasks.API.Common.SDSCombinators
import iTasks._Framework.Tonic
import iTasks.UI.Layout, iTasks.UI.Editor, iTasks.UI.Prompt, iTasks.UI.Editor.Builtin

derive class iTask ChoiceGrid, ChoiceNode

enterInformation :: !d ![EnterOption m] -> Task m | toPrompt d & iTask m
enterInformation d [EnterWith fromf:_]
	= interact d Enter null (const ((),defaultValue)) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing)) Nothing @ (\((),v) -> fromf v) 
enterInformation d _ = enterInformation d [EnterWith id]

updateInformation :: !d ![UpdateOption m m] m -> Task m | toPrompt d & iTask m
updateInformation d [UpdateWith tof fromf:_] m
	= interact d Update null (const ((),tof m)) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing))
		Nothing @ (\((),v) -> fromf m v)
updateInformation d [UpdateUsing tof fromf editor:_] m
	= interact d Update null (const ((),tof m)) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing))
		(Just editor) @ (\((),v) -> fromf m v)
updateInformation d _ m = updateInformation d [UpdateWith (\l -> l) (\_ v -> v)] m

viewInformation :: !d ![ViewOption m] !m -> Task m | toPrompt d & iTask m
viewInformation d [ViewWith tof:_] m 
	= interact d View null (const ((),tof m)) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing)) Nothing @! m
viewInformation d [ViewUsing tof editor:_] m
	= interact d View null (const ((),tof m)) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing)) (Just editor) @! m
viewInformation d _ m = viewInformation d [ViewWith id] m

updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r & iTask w
updateSharedInformation d [UpdateWith tof fromf:_] shared
	= interact d Update shared (\r -> (r, tof r))
				(\v l _ -> (l,v,Just (\r -> fromf r v)))
				(\r _ v -> (r,tof r,Nothing))
				Nothing @ fst

updateSharedInformation d [UpdateUsing tof fromf editor:_] shared
	= interact d Update shared (\r -> (r,tof r))
				(\v l _ -> (l,v,Just (\r -> fromf r v)))
				(\r _ v -> (r,tof r,Nothing))
				(Just editor) @ fst

updateSharedInformation d [UpdateWithShared tof fromf conflictf:_] shared
	= interact d Update shared (\r -> (r,tof r))
				(\v l _ -> (l,v,Just (\r -> fromf r v)))
				(\r _ v -> (r,conflictf (tof r) v, Nothing))
				Nothing @ fst

updateSharedInformation d _ shared			
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we just display the read type r 
	= case dynamic id :: A.a: (a -> a) of
		(rtow :: r^ -> w^)
			= updateSharedInformation d [UpdateWith rtow (\_ v -> v)] shared 
		_
			= viewSharedInformation d [] shared

viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r
viewSharedInformation d [ViewWith tof:_] shared
	= interact d View shared (\r -> (r,tof r))
				(\v l _ -> (l,v,Nothing))
				(\r _ v -> (r,tof r,Nothing)) 
				Nothing @ fst
viewSharedInformation d _ shared = viewSharedInformation d [ViewWith id] shared

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | toPrompt d & iTask r & iTask m
updateInformationWithShared d [UpdateWith tof fromf:_] shared m
	= interact d Update shared (\r -> ((r,m),tof (r,m)))
				(\v (r,m) _ -> let nm = fromf (r,m) v in ((r,nm),v,Nothing))
				(\r (_,m) v -> ((r,m),tof (r,m),Nothing))
				Nothing @ (snd o fst)
updateInformationWithShared d [UpdateUsing tof fromf editor:_] shared m
	= interact d Update shared (\r -> ((r,m),tof (r,m)))
		(\v (r,m) _ -> let nm = fromf (r,m) v in ((r,nm),v,Nothing))
		(\r (_,m) v -> ((r,m),tof (r,m),Nothing))
		(Just editor) @ (snd o fst)
updateInformationWithShared d _ shared m
    = updateInformation d [] m


editSelection :: !d !(SelectOption c a) c [Int] -> Task [a] | toPrompt d & iTask a
editSelection d (SelectInDropdown toView fromView) container sel = editSelection` d dropdown toView fromView container sel
editSelection d (SelectInRadioGroup toView fromView) container sel = editSelection` d radioGroup toView fromView container sel
editSelection d (SelectInList toView fromView) container sel = editSelection` d choiceList toView fromView container sel
editSelection d (SelectInGrid toView fromView) container sel = editSelection` d grid toView fromView container sel
editSelection d (SelectInTree toView fromView) container sel = editSelection` d tree toView fromView container sel
editSelection` d editor toView fromView container sel
	= interact d (if (isEmpty sel) Enter Update) null
		(\r     -> ((),(toView container,sel)))
		(\v l _ -> (l,v,Nothing))
		(\_ l v -> (l,v,Nothing))
		(Just editor) @ (\(_,(_,sel)) -> fromView container sel)

editSelectionWithShared :: !d !(SelectOption c a) (ReadWriteShared c w) (c -> [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 
editSelectionWithShared d (SelectInDropdown toView fromView) sharedContainer initSel = editSelectionWithShared` d dropdown toView fromView sharedContainer initSel
editSelectionWithShared d (SelectInRadioGroup toView fromView) sharedContainer initSel = editSelectionWithShared` d radioGroup toView fromView sharedContainer initSel
editSelectionWithShared d (SelectInList toView fromView) sharedContainer initSel = editSelectionWithShared` d choiceList toView fromView sharedContainer initSel
editSelectionWithShared d (SelectInGrid toView fromView) sharedContainer initSel = editSelectionWithShared` d grid toView fromView sharedContainer initSel
editSelectionWithShared d (SelectInTree toView fromView) sharedContainer initSel = editSelectionWithShared` d tree toView fromView sharedContainer initSel
editSelectionWithShared` d editor toView fromView sharedContainer initSel
	= interact d Update sharedContainer 
		(\r     -> (r,(toView r, initSel r)))
		(\v l _ -> (l,v,Nothing))
		(\r l (v,sel) -> (r,(v,sel),Nothing))
		(Just editor) @ (\(container,(_,sel)) -> fromView container sel)

editSharedSelection :: !d !(SelectOption c a) c (Shared [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 
editSharedSelection d (SelectInDropdown toView fromView) container sharedSel = editSharedSelection` d dropdown toView fromView container sharedSel
editSharedSelection d (SelectInRadioGroup toView fromView) container sharedSel = editSharedSelection` d radioGroup toView fromView container sharedSel
editSharedSelection d (SelectInList toView fromView) container sharedSel = editSharedSelection` d choiceList toView fromView container sharedSel
editSharedSelection d (SelectInGrid toView fromView) container sharedSel = editSharedSelection` d grid toView fromView container sharedSel
editSharedSelection d (SelectInTree toView fromView) container sharedSel = editSharedSelection` d tree toView fromView container sharedSel
editSharedSelection` d editor toView fromView container sharedSel 
	= interact d Update sharedSel
		(\r           -> ((),(toView container,r)))
		(\(vt,vs) l _ -> (l,(vt,vs),Just (const vs)))
		(\r l (vt,vs) -> (l,(vt,r),Nothing))
		(Just editor) @ (\(_,(_,sel)) -> fromView container sel)

editSharedSelectionWithShared :: !d !(SelectOption c a) (ReadWriteShared c w) (Shared [Int]) -> Task [a] | toPrompt d & iTask c & iTask a 
editSharedSelectionWithShared d (SelectInDropdown toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d dropdown toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d (SelectInRadioGroup toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d radioGroup toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d (SelectInList toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d choiceList toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d (SelectInGrid toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d grid toView fromView sharedContainer sharedSel
editSharedSelectionWithShared d (SelectInTree toView fromView) sharedContainer sharedSel 
	= editSharedSelectionWithShared` d tree toView fromView sharedContainer sharedSel
editSharedSelectionWithShared` d editor toView fromView sharedContainer sharedSel 
	= interact d Update (sharedContainer |+< sharedSel)
		(\(rc,rs)           -> (rc,(toView rc,rs)))
		(\(vt,vs) l _       -> (l,(vt,vs),Just (const vs)))
		(\(rc,rs) l (vt,vs) -> (l,(toView rc,rs),Nothing))
		(Just editor) @ (\(container,(_,sel)) -> fromView container sel)

//Core choice tasks

editChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask a
editChoiceAs d vopts container target mbSel = editSelection d (selectOption target vopts) container (findIdx target mbSel container) @? tvHd

editChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editChoiceWithSharedAs d vopts sharedContainer target mbSel 
	= editSelectionWithShared d (selectOption target vopts) sharedContainer (findIdx target mbSel) @? tvHd

editChoiceSimple :: !d ![o] (Maybe o) -> Task o | toPrompt d & iTask o
editChoiceSimple d container mbSel = editSelection d (selectOption id []) container (findIdx id mbSel container) @? tvHd

editChoiceWithSharedSimple :: !d !(ReadWriteShared [o] w) (Maybe o) -> Task o | toPrompt d & iTask o & iTask w
editChoiceWithSharedSimple d sharedContainer mbSel
	= editSelectionWithShared d (selectOption id []) sharedContainer (findIdx id mbSel) @? tvHd

editSharedChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask a
editSharedChoiceAs d vopts container target sharedSel 
	= editSharedSelection d (selectOption target vopts) container (findIdxShare target container sharedSel) @? tvHd

editSharedChoiceSimple :: !d ![o] (Shared (Maybe o)) -> Task o | toPrompt d & iTask o
editSharedChoiceSimple d container sharedSel
	= editSharedSelection d (selectOption id []) container (findIdxShare id container sharedSel) @? tvHd

editSharedChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editSharedChoiceWithSharedAs d vopts sharedContainer target sharedSel
	= editSharedSelectionWithShared d (selectOption target vopts) sharedContainer (findIdxShareWithShared target (sharedContainer |+< sharedSel)) @? tvHd

editSharedChoiceWithSharedSimple :: !d !(ReadWriteShared [o] w) (Shared (Maybe o)) -> Task o | toPrompt d & iTask o & iTask w
editSharedChoiceWithSharedSimple d sharedContainer sharedSel
	= editSharedSelectionWithShared d (selectOption id []) sharedContainer (findIdxShareWithShared id (sharedContainer |+< sharedSel)) @? tvHd

//Helper functions for the edit*Choice* tasks
selectOption target opts = case opts of
	[ChooseWith (ChooseFromDropdown f):_]     = SelectInDropdown (toLabels f) (findSelection target)
	[ChooseWith (ChooseFromRadioButtons f):_] = SelectInRadioGroup (toLabels f) (findSelection target)
	[ChooseWith (ChooseFromList f):_]         = SelectInList (toLabels f) (findSelection target)
	[ChooseWith (ChooseFromGrid f):_]         = SelectInGrid (toGrid f) (findSelection target)
	_                                         = SelectInDropdown (toLabels id) (findSelection target) 
where
	toLabels f options = map (toSingleLineText o f) options
	toGrid f options = {ChoiceGrid|header=gText{|*|} AsHeader (fix vals),rows = [map Text (gText{|*|} AsRow (Just v)) \\ v <- vals]}
	where
		vals = map f options

		fix :: [a] -> Maybe a
		fix _ = Nothing

findSelection target options [idx] = [target (options !! idx)]
findSelection target options _     = []

findIdx target Nothing options = []
findIdx target (Just val) options = [i \\ o <- options & i <- [0..] | target o === val]

findIdxShare target options sds = mapReadWrite (tof,fromf) sds
where
	tof mbv = findIdx target mbv options
	fromf w _ = Just (listToMaybe (findSelection target options w))

findIdxShareWithShared target sds = mapReadWrite (tof,fromf) sds
where
	tof (options,mbv) = findIdx target mbv options
	fromf w (options,_) = Just (listToMaybe (findSelection target options w))

//More convenient versions 
editChoice :: !d ![ChoiceOption a] ![a] (Maybe a) -> Task a | toPrompt d & iTask a
editChoice d [] container mbSel = editChoiceSimple d container mbSel
editChoice d options container mbSel = editChoiceAs d options container id mbSel

enterChoice :: !d ![ChoiceOption a] ![a] -> Task a | toPrompt d & iTask a
enterChoice d options container = editChoice d options container Nothing

enterChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) -> Task a | toPrompt d & iTask o & iTask a
enterChoiceAs d options container targetFun = editChoiceAs d options container targetFun Nothing

updateChoice :: !d ![ChoiceOption a] ![a] a -> Task a | toPrompt d & iTask a
updateChoice d options container sel = editChoice d options container (Just sel)

updateChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) a -> Task a | toPrompt d & iTask o & iTask a
updateChoiceAs d options container targetFun sel = editChoiceAs d options container targetFun (Just sel)

editChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Maybe a) -> Task a | toPrompt d & iTask a & iTask w
editChoiceWithShared d [] container mbSel = editChoiceWithSharedSimple d container mbSel
editChoiceWithShared d options container mbSel = editChoiceWithSharedAs d options container id mbSel

enterChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task a | toPrompt d & iTask a & iTask w
enterChoiceWithShared d options container = editChoiceWithShared d options container Nothing

enterChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
enterChoiceWithSharedAs d options container targetFun = editChoiceWithSharedAs d options container targetFun Nothing

updateChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) a -> Task a | toPrompt d & iTask a & iTask w
updateChoiceWithShared d options container sel = editChoiceWithShared d options container (Just sel)

updateChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) a -> Task a | toPrompt d & iTask o & iTask w & iTask a
updateChoiceWithSharedAs d options container targetFun sel = editChoiceWithSharedAs d options container targetFun (Just sel)

editSharedChoice :: !d ![ChoiceOption a] ![a] (Shared (Maybe a)) -> Task a | toPrompt d & iTask a
editSharedChoice d [] container sharedSel = editSharedChoiceSimple d container sharedSel
editSharedChoice d options container sharedSel = editSharedChoiceAs d options container id sharedSel

editSharedChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared (Maybe a)) -> Task a | toPrompt d & iTask a & iTask w
editSharedChoiceWithShared d [] sharedContainer sharedSel = editSharedChoiceWithSharedSimple d sharedContainer sharedSel
editSharedChoiceWithShared d options sharedContainer sharedSel = editSharedChoiceWithSharedAs d options sharedContainer id sharedSel

enterMultipleChoice :: !d ![MultiChoiceOption o] ![o] -> Task [o] | toPrompt d & iTask o
enterMultipleChoice d options container
	= updateInformation d (multiChoiceToUpdate options) (container,[]) @ snd

updateMultipleChoice :: !d ![MultiChoiceOption o] ![o] [o] -> Task [o] | toPrompt d & iTask o
updateMultipleChoice d options container sel
	= updateInformation d (multiChoiceToUpdate options) (container,sel) @ snd

enterSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) -> Task [o] | toPrompt d & iTask o & iTask w
enterSharedMultipleChoice d options shared
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared []

updateSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) [o] -> Task [o] | toPrompt d & iTask o & iTask w
updateSharedMultipleChoice d options shared sel 
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared sel

wait :: !d (r -> Bool) !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r
wait desc pred shared
	=	viewSharedInformation desc [ViewWith (const "Waiting for information update")] shared
	>>* [OnValue (ifValue pred return)]
	
waitForTime :: !Time -> Task Time
waitForTime time =
	viewSharedInformation ("Wait for time", ("Wait until " +++ toString time)) [] currentTime >>* [OnValue (ifValue (\now -> time < now) return)]

waitForDate :: !Date -> Task Date
waitForDate date =
	viewSharedInformation ("Wait for date", ("Wait until " +++ toString date)) [] currentDate >>* [OnValue (ifValue (\now -> date < now) return)]
	
waitForDateTime :: !DateTime -> Task DateTime
waitForDateTime datetime =
	viewSharedInformation ("Wait for date and time", ("Wait until " +++ toString datetime)) [] currentDateTime >>* [OnValue (ifValue (\now -> datetime < now) return)]

waitForTimer :: !Time -> Task Time
waitForTimer time = get currentTime >>- \now -> waitForTime (now + time)

chooseAction :: ![(!Action,a)] -> Task a | iTask a
chooseAction actions
	=	viewInformation () [] ()
	>>* [OnAction action (always (return val)) \\ (action,val) <- actions]

multiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption ([o], [o]) ([o],[o])] | iTask o
multiChoiceToUpdate [ChooseMultipleWith type view:_] = [UpdateWith (toView type) fromView]
where
	toView type (options,sel)	= selectOptions sel (initChoice type options)

	initChoice AutoMultiChoice options      = initChoice ChooseFromCheckBoxes options
	initChoice ChooseFromCheckBoxes options = CheckMultiChoice [(view o,o) \\ o <- options] []
	
	fromView (options,_) choice = (options,getSelections choice)
	
multiChoiceToUpdate _ = multiChoiceToUpdate [ChooseMultipleWith AutoMultiChoice id]

sharedMultiChoiceToUpdate :: [MultiChoiceOption o] -> [UpdateOption ([o], [o]) [o]] | iTask o
sharedMultiChoiceToUpdate options = case multiChoiceToUpdate options of
	[UpdateWith fromf tof]	= [UpdateWith fromf (\m v -> snd (tof m v))]
	_						= []

viewTitle :: !a -> Task a | iTask a
viewTitle a = viewInformation (Title title) [ViewWith view] a
where
	title = toSingleLineText a
	view a	= DivTag [] [SpanTag [StyleAttr "font-size: 30px"] [Text title]]

viewSharedTitle :: !(ReadWriteShared r w) -> Task r | iTask r
viewSharedTitle s = whileUnchanged s viewTitle

crudWith :: !d ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (RWShared () (f r) (f` w))
         -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w)
crudWith descr choiceOpts enterOpts viewOpts updateOpts toList putItem delItem sh = goCRUD
  where
  goCRUD
    =   enterChoiceWithShared descr choiceOpts (mapRead toList sh)
    >>* [ OnAction (Action "New" [])    (always   newItem)
        , OnAction (Action "View" [])   (hasValue viewItem)
        , OnAction (Action "Edit" [])   (hasValue editItem)
        , OnAction (Action "Delete" []) (hasValue deleteItem)
        ]
  newItem
    =            enterInformation (Title "New item") enterOpts
    >>= \item -> upd (putItem item) sh
    >>|          goCRUD
  viewItem x
    =            viewInformation (Title "View item") viewOpts x
    >>|          goCRUD
  editItem x
    =            updateInformation (Title "Edit item") updateOpts x
    >>= \item -> upd (putItem item) sh
    >>|          goCRUD
  deleteItem x
    =            upd (delItem x) sh
    >>|          goCRUD

crud :: !d !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
        (RWShared () (f r) (f` w))
     -> Task r | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w)
crud descr toList putItem delItem sh = crudWith descr [] [] [] [] toList putItem delItem sh
