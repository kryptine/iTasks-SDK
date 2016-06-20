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
import iTasks.UI.Layout, iTasks.UI.Editor, iTasks.UI.Prompt

enterInformation :: !d ![EnterOption m] -> Task m | toPrompt d & iTask m
enterInformation d [EnterWith fromf:_]
	= interact d Enter null () defaultValue (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing)) Nothing @ (\((),v) -> fromf v) 
enterInformation d _ = enterInformation d [EnterWith id]

updateInformation :: !d ![UpdateOption m m] m -> Task m | toPrompt d & iTask m
updateInformation d [UpdateWith tof fromf:_] m
	= interact d Update null () (tof m) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing))
		Nothing @ (\((),v) -> fromf m v)
updateInformation d [UpdateUsing tof fromf editor:_] m
	= interact d Update null () (tof m) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing))
		(Just editor) @ (\((),v) -> fromf m v)
updateInformation d _ m = updateInformation d [UpdateWith (\l -> l) (\_ v -> v)] m

viewInformation :: !d ![ViewOption m] !m -> Task m | toPrompt d & iTask m
viewInformation d [ViewWith tof:_] m 
	= interact d View null () (tof m) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing)) Nothing @! m
viewInformation d [ViewUsing tof editor:_] m
	= interact d View null () (tof m) (\v l _ -> (l,v,Nothing)) (\r l v -> (l,v,Nothing)) (Just editor) @! m
viewInformation d _ m = viewInformation d [ViewWith id] m

updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task r | toPrompt d & iTask r & iTask w
updateSharedInformation d [UpdateWith tof fromf:_] shared
	= interact d Update shared defaultValue defaultValue
				(\v l _ -> (l,v,Just (\r -> fromf r v)))
				(\r _ v -> (r,tof r,Nothing))
				Nothing @ fst

updateSharedInformation d [UpdateUsing tof fromf editor:_] shared
	= interact d Update shared defaultValue defaultValue
				(\v l _ -> (l,v,Just (\r -> fromf r v)))
				(\r _ v -> (r,tof r,Nothing))
				(Just editor) @ fst

updateSharedInformation d [UpdateWithShared tof fromf conflictf:_] shared
	= interact d Update shared defaultValue defaultValue
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
	= interact d View shared defaultValue defaultValue
				(\v l _ -> (l,v,Nothing))
				(\r _ v -> (r,tof r,Nothing)) 
				Nothing @ fst
viewSharedInformation d _ shared = viewSharedInformation d [ViewWith id] shared

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | toPrompt d & iTask r & iTask m
updateInformationWithShared d [UpdateWith tof fromf:_] shared m
	= interact d Update shared (defaultValue,m) defaultValue
				(\v (r,m) _ -> let nm = fromf (r,m) v in ((r,nm),v,Nothing))
				(\r (_,m) v -> ((r,m),tof (r,m),Nothing))
				Nothing @ (snd o fst)
updateInformationWithShared d [UpdateUsing tof fromf editor:_] shared m
	= interact d Update shared (defaultValue,m) defaultValue
		(\v (r,m) _ -> let nm = fromf (r,m) v in ((r,nm),v,Nothing))
		(\r (_,m) v -> ((r,m),tof (r,m),Nothing))
		(Just editor) @ (snd o fst)
updateInformationWithShared d _ shared m
    = updateInformation d [] m

//Core choice tasks
editChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask a
editChoiceAs d [ChooseWith type:_] container target mbSel
	= interact d (if (isNothing mbSel) Enter Update) null (map target container) (initChoiceView type container target mbSel)
		(\v l _ -> (l,v,Nothing))
		(\_ l v -> (l,v,Nothing))
		Nothing @? choiceRes
editChoiceAs d _ container target mbSel = editChoiceAs d [ChooseWith (AutoChoice id)] container target mbSel

editChoiceSimple :: !d ![o] (Maybe o) -> Task o | toPrompt d & iTask o
editChoiceSimple d container mbSel
	= interact d (if (isNothing mbSel) Enter Update) null () (initSimpleChoiceView container mbSel)
		(\v l _ -> (l,v,Nothing))
		(\_ l v -> (l,v,Nothing))
		Nothing @ snd @? simpleChoiceRes

editChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Maybe a) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editChoiceWithSharedAs d [ChooseWith type:_] sharedContainer target mbSel
    = interact d Update sharedContainer [] (initChoiceView type [] target mbSel)
		(\v l _ -> (l,v,Nothing))
		(\r l v -> (map target r,updateChoiceView type r target (selectionFromChoiceView l v) l v,Nothing))	
		Nothing @? choiceRes
editChoiceWithSharedAs d _ container target mbSel = editChoiceWithSharedAs d [ChooseWith (AutoChoice id)] container target mbSel

editChoiceWithSharedSimple :: !d !(ReadWriteShared [o] w) (Maybe o) -> Task o | toPrompt d & iTask o & iTask w
editChoiceWithSharedSimple d sharedContainer mbSel
	= interact d (if (isNothing mbSel) Enter Update) sharedContainer [] (initSimpleChoiceView [] mbSel)
		(\v l _ -> (l,v,Nothing))
		(\r l v -> (r,updateSimpleChoiceView r (getSelectionView v) v,Nothing))	
		Nothing @ snd @? simpleChoiceRes

editSharedChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask a
editSharedChoiceAs d [ChooseWith type:_] container target sharedSel 
	= interact d Update sharedSel (map target container) (initChoiceView type container target Nothing)
		(\v l _ -> (l,v,Nothing))
		(\r l v -> (l,updateChoiceSelection r l v,Nothing))
		Nothing @? choiceRes
editSharedChoiceAs d _ container target sharedSel = editSharedChoiceAs d [ChooseWith (AutoChoice id)] container target sharedSel

editSharedChoiceSimple :: !d ![o] (Shared (Maybe o)) -> Task o | toPrompt d & iTask o
editSharedChoiceSimple d container sharedSel
	= interact d Update sharedSel () (initSimpleChoiceView container Nothing)
		(\v l _ -> (l,v,Nothing))
		(\r l v -> (l,updateSimpleChoiceSelection r v,Nothing))
		Nothing @ snd @? simpleChoiceRes

editSharedChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared (Maybe a)) -> Task a | toPrompt d & iTask o & iTask w & iTask a
editSharedChoiceWithSharedAs d [ChooseWith type:_] sharedContainer target sharedSel
	= interact d Update (sharedContainer |+< sharedSel) [] (initChoiceView type [] target Nothing)
		(\v l _ -> (l,v,Nothing))
		(\(rc,rs) l v -> (map target rc,updateChoiceView type rc target rs l v,Nothing))
		Nothing @? choiceRes
editSharedChoiceWithSharedAs d _ sharedContainer target sharedSel = editSharedChoiceWithSharedAs d [ChooseWith (AutoChoice id)] sharedContainer target sharedSel

editSharedChoiceWithSharedSimple :: !d !(ReadWriteShared [o] w) (Shared (Maybe o)) -> Task o | toPrompt d & iTask o & iTask w
editSharedChoiceWithSharedSimple d sharedContainer sharedSel
	= interact d Update (sharedContainer |+< sharedSel) () (initSimpleChoiceView [] Nothing)
		(\v l _ -> (l,v,Nothing))
		(\(rc,rs) l v -> (l,updateSimpleChoiceView rc rs v,Nothing))
		Nothing @ snd @? simpleChoiceRes

initChoiceView :: (ChoiceType o v) [o] (o -> a) (Maybe a) -> (DynamicChoice v) | iTask o & iTask v & iTask a
initChoiceView type container target mbSel
    = updateChoiceSelection mbSel (map target container) (mkDynChoice type container)
where
    mkDynChoice (AutoChoice view) container             = mkDynChoice (autoChoiceType view container) container
    mkDynChoice (ChooseFromComboBox view) container     = DCCombo   (ComboChoice [view o \\ o <- container] Nothing)
    mkDynChoice (ChooseFromRadioButtons view) container = DCRadio   (RadioChoice [view o \\ o <- container] Nothing)
    mkDynChoice (ChooseFromList view) container         = DCList    (ListChoice  [view o \\ o <- container] Nothing)
    mkDynChoice (ChooseFromTree view) container         = DCTree    (TreeChoice  (view [(i,o) \\ i <- [0..] & o <- container] []) Nothing)
    mkDynChoice (ChooseFromGrid view) container         = DCGrid    (GridChoice [view o \\ o <- container] Nothing)

    autoChoiceType f l = case headers l defaultValue of
        []   = ChooseFromComboBox f
        [""] = ChooseFromComboBox f
        _    = ChooseFromGrid f

headers :: [a] a -> [String] | JSONEncode{|*|} a
headers _ a = case toJSON a of (JSONObject fields) = map fst fields ; _ = []

//When we don't have an (o -> a) transformation and no view transformation, we don't need to keep
//the choice options in the interact's state (which saves space and time)
initSimpleChoiceView :: [o] (Maybe o) -> (DynamicChoice o) | iTask o
initSimpleChoiceView container mbSel = updateChoiceSelection mbSel container (mkDynChoice container)
where
    mkDynChoice l = case headers l defaultValue of
        []   = DCCombo (ComboChoice container Nothing)
        [""] = DCCombo (ComboChoice container Nothing)
        _    = DCGrid  (GridChoice container Nothing)

updateChoiceView :: (ChoiceType o v) [o] (o -> a) (Maybe a) [a] (DynamicChoice v) -> (DynamicChoice v) | iTask o & iTask v & iTask a
//updateChoiceView type container target mbSel targets (view,mask)
    //Crude solution, just rebuild view and update (will do for now)
 //   = initChoiceView type container target mbSel
updateChoiceView type container target mbSel targets view
    = updateChoiceSelection mbSel (map target container) (updDynChoice type targets view)
where
    updDynChoice (AutoChoice view) _ (DCCombo _)    = DCCombo (ComboChoice [view o \\ o <- container] Nothing)
    updDynChoice (AutoChoice view) _ (DCGrid _)     = DCGrid  (GridChoice [view o \\ o <- container] Nothing)

    updDynChoice (ChooseFromComboBox view) _ _      = DCCombo (ComboChoice [view o \\ o <- container] Nothing)
    updDynChoice (ChooseFromRadioButtons view) _ _  = DCRadio (RadioChoice [view o \\ o <- container] Nothing)
    updDynChoice (ChooseFromTree view) targets (DCTree (TreeChoice tree _))
        = DCTree  (TreeChoice  (view [(i,o) \\ i <- [0..] & o <- container] (expanded tree targets container)) Nothing)
    updDynChoice (ChooseFromTree view) _ _          = DCTree  (TreeChoice  (view [(i,o) \\ i <- [0..] & o <- container] []) Nothing)
    updDynChoice (ChooseFromGrid view) _ _          = DCGrid  (GridChoice [view o \\ o <- container] Nothing)
    updDynChoice (ChooseFromList view) _ _          = DCList  (ListChoice [view o \\ o <- container] Nothing)

    expanded tree targets container = remapChoiceIndices targets (map target container) (expandedFromTreeChoiceView tree)

updateSimpleChoiceView :: [o] (Maybe o) (DynamicChoice o) -> (DynamicChoice o) | iTask o
updateSimpleChoiceView container mbSel view = initSimpleChoiceView container mbSel

updateChoiceSelection :: (Maybe a) [a] (DynamicChoice v) -> (DynamicChoice v) | iTask v & iTask a
updateChoiceSelection Nothing    targets dynChoice = setSelectionIndex Nothing dynChoice
updateChoiceSelection (Just sel) targets dynChoice = setSelectionIndex (findIndex ((===) sel) targets) dynChoice

updateSimpleChoiceSelection :: (Maybe o) (DynamicChoice o) -> (DynamicChoice o) | iTask o
updateSimpleChoiceSelection mbSel dynChoice = (setSelectionView mbSel dynChoice)

choiceRes :: (TaskValue ([a],DynamicChoice v)) -> TaskValue a
choiceRes (Value (targets,view) _) = case selectionFromChoiceView targets view of
    Just x  = Value x False
    _       = NoValue
choiceRes _ = NoValue

simpleChoiceRes :: (TaskValue (DynamicChoice a)) -> TaskValue a
simpleChoiceRes (Value view _) = case getSelectionView view of
    Just x          = Value x False
    _               = NoValue
simpleChoiceRes _   = NoValue

mapSharedSel :: (TaskValue ([a],DynamicChoice v)) (Maybe a) -> (Maybe (Maybe a)) | gEq{|*|} a
mapSharedSel (Value (targets,view) _) (Just ov) = maybe Nothing (\idx -> let nv = targets !! idx in if (nv =!= ov)  (Just (Just nv)) Nothing) (getSelectionIndex view)
mapSharedSel (Value (targets,view) _) _         = fmap (\idx -> Just (targets !! idx)) (getSelectionIndex view)
mapSharedSel _ _                                = Nothing

mapSimpleSharedSel :: (TaskValue (DynamicChoice a)) (Maybe a) -> (Maybe (Maybe a)) | gEq{|*|} a
mapSimpleSharedSel (Value view _) (Just ov)     = maybe Nothing (\nv -> if (nv =!= ov) (Just (Just nv)) Nothing) (getSelectionView view)
mapSimpleSharedSel (Value view _) _             = fmap Just (getSelectionView view)
mapSimpleSharedSel _ _                          = Nothing

selectionFromChoiceView :: [a] (DynamicChoice v) -> (Maybe a)
selectionFromChoiceView targets dynChoice = fmap (\idx -> targets !! idx) (getSelectionIndex dynChoice)

expandedFromTreeChoiceView :: [ChoiceTree a] -> [ChoiceTreeValue]
expandedFromTreeChoiceView nodes = foldr add [] nodes
where
    add {ChoiceTree|value,type=ExpandedNode children} expanded = [value:expandedFromTreeChoiceView children ++ expanded]
    add {ChoiceTree|value,type=CollapsedNode children} expanded = expandedFromTreeChoiceView children ++ expanded
    add _ expanded = expanded

//When the list of things to choose from changes, the old indices have to be remapped to the list to choose from
remapChoiceIndices :: [a] [a] [ChoiceTreeValue] -> [ChoiceTreeValue] | gEq{|*|} a //TODO: This should be possible in linear time
remapChoiceIndices old new [] = []
remapChoiceIndices old new [ChoiceNode i:ns] = let ns` = remapChoiceIndices old new ns in maybe ns` (\i` -> [ChoiceNode i`:ns`]) (findIndex ((===) (old !! i)) new)
remapChoiceIndices old new [GroupNode l:ns] = [GroupNode l:remapChoiceIndices old new ns]

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
viewTitle a = viewInformation (Title title) [ViewWith view] a //<<@ InContainer //<<@ AfterLayout (tweakAttr titleFromValue)
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
