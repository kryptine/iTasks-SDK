implementation module iTasks.API.Common.InteractionTasks

from StdFunc import id, const, o, flip
from iTasks.API.Core.SDSs import null
from Data.Tuple import appSnd
from Data.List import isMemberGen, findIndex, instance Functor []
from System.Time import :: Timestamp(..)
from Data.Map import qualified get, put

import StdBool, StdList, StdMisc, StdTuple, Data.Functor
import iTasks.API.Core.Tasks, iTasks.API.Core.OptimizedCoreTasks, iTasks.API.Core.TaskCombinators
import iTasks.API.Common.TaskCombinators, iTasks.API.Core.SDSs
import iTasks._Framework.Generic.Interaction, iTasks.API.Common.SDSCombinators
import iTasks._Framework.Tonic
import iTasks.UI.Layout

/*
editInformation :: !d ![UpdateOption m m] (Maybe m) -> Task m | descr d & iTask m
editInformation d [UpdateWith tof fromf:_] m
	= interact d null
		(\r -> let v = tof m in (m,(v,Untouched)))
		(\l r (v,m) rCh vCh vOk -> if vOk (let nl = fromf l v in (let nv = tof nl in (nl,(nv,m)))) (l,(v,m)))
where	
    initialValue Nothing        = (defaultValue,Untouched)
    initialValue (Just value)   = (value,Touched)

editInformation d _ m = editInformation d [UpdateWith (\l -> l) (\_ v -> v)] m
*/


enterInformation :: !d ![EnterOption m] -> Task m | descr d & iTask m
enterInformation d [EnterWith fromf:_]
/*
	= interact d null
//		(\r -> (defaultValue,defaultValue,Untouched))
		(\r -> let v = defaultValue in (fromf v,v,Untouched))
		(\l r v m ok -> if ok (fromf v,v,m) (l,v,m))
*/
	= interactNullEnter d defaultValue fromf
enterInformation d _ = enterInformation d [EnterWith id]

updateInformation :: !d ![UpdateOption m m] m -> Task m | descr d & iTask m
updateInformation d [UpdateWith tof fromf:_] m
	= interact d null
		(\r -> let v = tof m in (m,(v,Touched)))
		(\l r (v,m) rCh vCh vOk -> if vOk (let nl = fromf l v in (let nv = tof nl in (nl,(nv,m)))) (l,(v,m)))

//TODO: THIS OPTIMIZATION IS WRONG!
//	= interactNullUpdate d tof fromf m
updateInformation d _ m = updateInformation d [UpdateWith (\l -> l) (\_ v -> v)] m

viewInformation :: !d ![ViewOption m] !m -> Task m | descr d & iTask m
viewInformation d [ViewWith tof:_] m
/*
	= interact d null
		(\r -> let v = Display (tof m) in (m,v,defaultMask v))
		(\l r v m ok -> (l,v,m))
*/
	= interactNullView d tof m
viewInformation d _ m = viewInformation d [ViewWith id] m

updateSharedInformation :: !d ![UpdateOption r w] !(ReadWriteShared r w) -> Task w | descr d & iTask r & iTask w
updateSharedInformation d [UpdateWith tof fromf:_] shared
	= interact d (toReadOnly shared)
				(\r -> let v = tof r in (fromf r v,(v,Touched)))
				(\l r (v,m) rCh vCh vOk -> if vOk
					(if rCh //If the share changed, refresh the view
						(let nv = tof r in (fromf r nv,(nv,Touched)))
						(fromf r v,(v,m))
					)
					(l,(v,m))
				)
				@> (mapval,shared)
updateSharedInformation d [UpdateWithShared tof fromf conflictf:_] shared
	= interact d (toReadOnly shared)
				(\r -> let v = tof r in (fromf r v,(v,Touched)))
				(\l r (v,m) rCh vCh vOk -> if vOk
					(if rCh 
                        (if vCh
                            //Both the share changed and the view changed -> resolve conflict
                            (let nv = conflictf v (tof r) in (fromf r nv,(nv,Touched)))
                            //Only the share changed, refresh the view
						    (let nv = tof r in (fromf r nv,(nv,Touched)))
                        )             
						(fromf r v,(v,m))
					)
					(l,(v,m))
				)
				@> (mapval,shared)
updateSharedInformation d _ shared			
	//Use dynamics to test if r == w, if so we can use an update view	
	//If different types are used we can only resort to a display of type r and an enter of type w
	= case dynamic id :: A.a: (a -> a) of
		(rtow :: (r^ -> w^))
			= interact d (toReadOnly shared)
				(\r -> let v = rtow r in (rtow r,(v,Touched)))
				(\l r (v,m) rCh vCh vOk -> if vOk (if (rtow r =!= l) (let nv = rtow r in (nv,(nv,Touched))) (v,(v,m))) (l,(v,m)))
				@> (mapval,shared)
		_
			= interact d (toReadOnly shared)
				(\r -> let v = (Display r,defaultValue) in (defaultValue,(v,CompoundMask [Touched,Untouched])))
				(\l r ((_,v),(CompoundMask [_,m])) rCh vCh vOk -> let nl = if vOk v l in (let nv = (Display r,nl) in (nl,(nv,CompoundMask [Touched,m]))))
				@> (mapval,shared)	

mapval (Value w _) _	= Just w
mapval _ _				= Nothing

viewSharedInformation :: !d ![ViewOption r] !(ReadWriteShared r w) -> Task r | descr d & iTask r
viewSharedInformation d [ViewWith tof:_] shared
/*
	= interact d (toReadOnly shared)
		(\r -> let v = Display (tof r) in (r,v,defaultMask v))
		(\l r v m ok -> let v = Display (tof r) in (r,v,defaultMask v)) 
*/
	= interactSharedInformation d (toReadOnly shared) (\r -> Display (tof r))
viewSharedInformation d _ shared = viewSharedInformation d [ViewWith id] shared

updateInformationWithShared :: !d ![UpdateOption (r,m) m] !(ReadWriteShared r w) m -> Task m | descr d & iTask r & iTask m
updateInformationWithShared d [UpdateWith tof fromf:_] shared m
	= interact d (toReadOnly shared)
		(\r -> let v = tof (r,m) in (m,(v,Touched)))
		(\l r (v,msk) rCh vCh vOk -> let nl = if vOk (fromf (r,l) v) l in (let v = tof (r,nl) in (nl,(v,Touched))))
updateInformationWithShared d _ shared m
	= interact d (toReadOnly shared)
		(\r -> let v = (Display r,m) in (m,(v,CompoundMask [Touched,Untouched])))
		(\l r ((_,v),(CompoundMask [_,msk])) rCh vCh vOk -> let nl = if vOk v l in (let nv = (Display r,nl) in (nl,(nv,CompoundMask [Touched,msk]))))

//Core choice tasks
editChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Maybe a) -> Task a | descr d & iTask o & iTask a
editChoiceAs d [ChooseWith type:_] container target mbSel
    = interactLocalExposed d (initFun type container target mbSel) updateFun @? choiceRes
where
    initFun :: (ChoiceType o v) [o] (o -> a) (Maybe a) -> ([a], (DynamicChoice v,InteractionMask)) | iTask o & iTask a & iTask v
    initFun type container target mbSel = (map target container, initChoiceView type container target mbSel)

    updateFun :: [a] (DynamicChoice v,InteractionMask) Bool -> ([a], (DynamicChoice v,InteractionMask))
    updateFun targets (view,mask) viewOk
        = (targets,(view,mask))
editChoiceAs d _ container target mbSel = editChoiceAs d [ChooseWith (AutoChoice id)] container target mbSel

editChoiceSimple :: !d ![o] (Maybe o) -> Task o | descr d & iTask o
editChoiceSimple d container mbSel
    = interactLocalViewOnly d (initSimpleChoiceView container mbSel) updateFun @? simpleChoiceRes
where
    updateFun :: (DynamicChoice o,InteractionMask) Bool -> (DynamicChoice o,InteractionMask)
    updateFun (view,mask) viewOk = (view,mask)

editChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Maybe a) -> Task a | descr d & iTask o & iTask w & iTask a
editChoiceWithSharedAs d [ChooseWith type:_] sharedContainer target mbSel
    = interactExposed d (toReadOnly sharedContainer) (initFun type target mbSel) (updateFun type target) @? choiceRes
where
    initFun :: (ChoiceType o v) (o -> a) (Maybe a) [o] -> ([a], (DynamicChoice v, InteractionMask)) | iTask o & iTask v & iTask a
    initFun type target mbSel container = (map target container,initChoiceView type container target mbSel)

    updateFun :: (ChoiceType o v) (o -> a) [a] [o] (DynamicChoice v,InteractionMask) Bool Bool Bool -> ([a], (DynamicChoice v,InteractionMask)) | iTask o & iTask v & iTask a
    updateFun type target targets container (view,mask) shareChanged viewChanged viewOk
        //First check if we changed the selection, then update the view
        | shareChanged
            # (view,mask)   = updateChoiceView type container target (selectionFromChoiceView targets view) targets (view,mask)
            # targets       = map target container
            = (targets,(view,mask))
        | otherwise
            = (targets,(view,mask))
editChoiceWithSharedAs d _ container target mbSel = editChoiceWithSharedAs d [ChooseWith (AutoChoice id)] container target mbSel

editChoiceWithSharedSimple :: !d !(ReadWriteShared [o] w) (Maybe o) -> Task o | descr d & iTask o & iTask w
editChoiceWithSharedSimple d sharedContainer mbSel
    = interactViewOnly d (toReadOnly sharedContainer) (initFun mbSel) updateFun @? simpleChoiceRes
where
    initFun :: (Maybe o) [o] -> (DynamicChoice o,InteractionMask) | iTask o
    initFun mbSel container = initSimpleChoiceView container mbSel

    updateFun :: [o] (DynamicChoice o,InteractionMask) Bool Bool Bool -> (DynamicChoice o,InteractionMask) | iTask o
    updateFun container (view,mask) shareChanged viewChanged viewOk
        | shareChanged
            = updateSimpleChoiceView container (getSelectionView view) (view,mask)
        | otherwise
            = (view,mask)

editSharedChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) (Shared (Maybe a)) -> Task a | descr d & iTask o & iTask a
editSharedChoiceAs d [ChooseWith type:_] container target sharedSel
    =  interactExposed d (toReadOnly sharedSel) (initFun type container target) updateFun
    @> (mapSharedSel,sharedSel)
    @? choiceRes
where
    initFun :: (ChoiceType o v) [o] (o -> a) (Maybe a) -> ([a], (DynamicChoice v,InteractionMask)) | iTask o & iTask v & iTask a
    initFun type container target mbSel = (map target container, initChoiceView type container target mbSel)

    updateFun :: [a] (Maybe a) (DynamicChoice v,InteractionMask) Bool Bool Bool -> ([a], (DynamicChoice v,InteractionMask)) | iTask v & iTask a
    updateFun targets mbSel (view,mask) shareChanged viewChanged viewOk
        # mbSel         = if (viewChanged && viewOk) (selectionFromChoiceView targets view) mbSel
        # (view,mask)   = if (shareChanged && not (viewChanged && viewOk)) (updateChoiceSelection mbSel targets (view,mask)) (view,mask)
        = (targets,(view,mask))
editSharedChoiceAs d _ container target sharedSel = editSharedChoiceAs d [ChooseWith (AutoChoice id)] container target sharedSel

editSharedChoiceSimple :: !d ![o] (Shared (Maybe o)) -> Task o | descr d & iTask o
editSharedChoiceSimple d container sharedSel
    = interactViewOnly d (toReadOnly sharedSel) (initSimpleChoiceView container) (updateFun container)
    @> (mapSimpleSharedSel,sharedSel)
    @? simpleChoiceRes
where
    updateFun :: [o] (Maybe o) (DynamicChoice o,InteractionMask) Bool Bool Bool -> (DynamicChoice o,InteractionMask) | iTask o
    updateFun container mbSel (view,mask) shareChanged viewChanged viewOk
        # mbSel         = if (viewChanged && viewOk) (getSelectionView view) mbSel
        = if (shareChanged && not (viewChanged && viewOk)) (updateSimpleChoiceSelection mbSel (view,mask)) (view,mask)

editSharedChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) (Shared (Maybe a)) -> Task a | descr d & iTask o & iTask w & iTask a
editSharedChoiceWithSharedAs d [ChooseWith type:_] sharedContainer target sharedSel
    =  interactExposed d (sharedContainer |+| sharedSel) (initFun type target) (updateFun type target)
    @> (mapSharedSel,sharedSel)
    @? choiceRes
where
    initFun :: (ChoiceType o v) (o -> a) ([o], Maybe a) -> ([a], (DynamicChoice v, InteractionMask)) | iTask o & iTask v & iTask a
    initFun type target (container,mbSel) = (map target container,initChoiceView type container target mbSel)

    updateFun :: (ChoiceType o v) (o -> a) [a] ([o], Maybe a) (DynamicChoice v,InteractionMask) Bool Bool Bool -> ([a], (DynamicChoice v,InteractionMask)) | iTask o & iTask v & iTask a
    updateFun type target targets (container,mbSel) (view,mask) shareChanged viewChanged viewOk
        # mbSel         = if (viewChanged && viewOk) (selectionFromChoiceView targets view) mbSel
        # (view,mask)   = if shareChanged (updateChoiceView type container target mbSel targets (view,mask)) (view,mask)
        # targets       = if shareChanged (map target container) targets
        = (targets,(view,mask))
editSharedChoiceWithSharedAs d _ sharedContainer target sharedSel = editSharedChoiceWithSharedAs d [ChooseWith (AutoChoice id)] sharedContainer target sharedSel

editSharedChoiceWithSharedSimple :: !d !(ReadWriteShared [o] w) (Shared (Maybe o)) -> Task o | descr d & iTask o & iTask w
editSharedChoiceWithSharedSimple d sharedContainer sharedSel
    = interactViewOnly d (sharedContainer |+| sharedSel) initFun updateFun
    @> (mapSimpleSharedSel,sharedSel)
    @? simpleChoiceRes
where
    initFun ::  ([o], Maybe o) -> (DynamicChoice o, InteractionMask) | iTask o
    initFun (container,mbSel) = initSimpleChoiceView container mbSel

    updateFun :: ([o], Maybe o) (DynamicChoice o,InteractionMask) Bool Bool Bool -> (DynamicChoice o,InteractionMask) | iTask o
    updateFun (container,mbSel) (view,mask) shareChanged viewChanged viewOk
        # mbSel         = if (viewChanged && viewOk) (getSelectionView view) mbSel
        # (view,mask)   = if shareChanged (updateSimpleChoiceView container mbSel (view,mask)) (view,mask)
        = (view,mask)

initChoiceView :: (ChoiceType o v) [o] (o -> a) (Maybe a) -> (DynamicChoice v,InteractionMask) | iTask o & iTask v & iTask a
initChoiceView type container target mbSel
    = updateChoiceSelection mbSel (map target container) (mkDynChoice type container,Untouched)
where
    mkDynChoice (AutoChoice view) container             = mkDynChoice (autoChoiceType view container) container
    mkDynChoice (ChooseFromComboBox view) container     = DCCombo   (ComboChoice [view o \\ o <- container] Nothing)
    mkDynChoice (ChooseFromRadioButtons view) container = DCRadio   (RadioChoice [view o \\ o <- container] Nothing)
    mkDynChoice (ChooseFromList view) container         = DCList    (ListChoice  [view o \\ o <- container] Nothing)
    mkDynChoice (ChooseFromTree view) container         = DCTree    (TreeChoice  (view [(i,o) \\ i <- [0..] & o <- container] []) Nothing)
    mkDynChoice (ChooseFromGrid view) container         = DCGrid    (GridChoice [view o \\ o <- container] Nothing)

    autoChoiceType f l = case headers l undef of
        []   = ChooseFromComboBox f
        [""] = ChooseFromComboBox f
        _    = ChooseFromGrid f
    where
        headers :: [a] a -> [String] | gEditMeta{|*|} a
        headers _ a = [fromMaybe "" label \\{EditMeta|label} <- gEditMeta{|*|} a]

//When we don't have an (o -> a) transformation and no view transformation, we don't need to keep
//the choice options in the interact's state (which saves space and time)
initSimpleChoiceView :: [o] (Maybe o) -> (DynamicChoice o, InteractionMask) | iTask o
initSimpleChoiceView container mbSel = updateChoiceSelection mbSel container (mkDynChoice container,Untouched)
where
    mkDynChoice l = case headers l undef of
        []   = DCCombo (ComboChoice container Nothing)
        [""] = DCCombo (ComboChoice container Nothing)
        _    = DCGrid  (GridChoice container Nothing)
    where
        headers :: [a] a -> [String] | gEditMeta{|*|} a
        headers _ a = [fromMaybe "" label \\{EditMeta|label} <- gEditMeta{|*|} a]

updateChoiceView :: (ChoiceType o v) [o] (o -> a) (Maybe a) [a] (DynamicChoice v,InteractionMask) -> (DynamicChoice v,InteractionMask) | iTask o & iTask v & iTask a
//updateChoiceView type container target mbSel targets (view,mask)
    //Crude solution, just rebuild view and update (will do for now)
 //   = initChoiceView type container target mbSel
updateChoiceView type container target mbSel targets (view,mask)
    = updateChoiceSelection mbSel (map target container) (updDynChoice type targets view,mask)
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

updateSimpleChoiceView :: [o] (Maybe o) (DynamicChoice o,InteractionMask) -> (DynamicChoice o,InteractionMask) | iTask o
updateSimpleChoiceView container mbSel (view,mask)
    = initSimpleChoiceView container mbSel

updateChoiceSelection :: (Maybe a) [a] (DynamicChoice v, InteractionMask) -> (DynamicChoice v, InteractionMask) | iTask v & iTask a
updateChoiceSelection Nothing    targets (dynChoice,_) = (setSelectionIndex Nothing dynChoice, Untouched)
updateChoiceSelection (Just sel) targets (dynChoice,_) = (setSelectionIndex (findIndex ((===) sel) targets) dynChoice, Touched)

updateSimpleChoiceSelection :: (Maybe o) (DynamicChoice o, InteractionMask) -> (DynamicChoice o, InteractionMask) | iTask o
updateSimpleChoiceSelection mbSel (dynChoice,_) = (setSelectionView mbSel dynChoice, if (isJust mbSel) Touched Untouched)

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

editChoice :: !d ![ChoiceOption a] ![a] (Maybe a) -> Task a | descr d & iTask a
editChoice d [] container mbSel = editChoiceSimple d container mbSel
editChoice d options container mbSel = editChoiceAs d options container id mbSel

enterChoice :: !d ![ChoiceOption a] ![a] -> Task a | descr d & iTask a
enterChoice d options container = editChoice d options container Nothing

enterChoiceAs :: !d [ChoiceOption o] ![o] !(o -> a) -> Task a | descr d & iTask o & iTask a
enterChoiceAs d options container targetFun = editChoiceAs d options container targetFun Nothing

updateChoice :: !d ![ChoiceOption a] ![a] a -> Task a | descr d & iTask a
updateChoice d options container sel = editChoice d options container (Just sel)

updateChoiceAs :: !d ![ChoiceOption o] ![o] !(o -> a) a -> Task a | descr d & iTask o & iTask a
updateChoiceAs d options container targetFun sel = editChoiceAs d options container targetFun (Just sel)

editChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Maybe a) -> Task a | descr d & iTask a & iTask w
editChoiceWithShared d [] container mbSel = editChoiceWithSharedSimple d container mbSel
editChoiceWithShared d options container mbSel = editChoiceWithSharedAs d options container id mbSel

enterChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) -> Task a | descr d & iTask a & iTask w
enterChoiceWithShared d options container = editChoiceWithShared d options container Nothing

enterChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) -> Task a | descr d & iTask o & iTask w & iTask a
enterChoiceWithSharedAs d options container targetFun = editChoiceWithSharedAs d options container targetFun Nothing

updateChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) a -> Task a | descr d & iTask a & iTask w
updateChoiceWithShared d options container sel = editChoiceWithShared d options container (Just sel)

updateChoiceWithSharedAs :: !d ![ChoiceOption o] !(ReadWriteShared [o] w) (o -> a) a -> Task a | descr d & iTask o & iTask w & iTask a
updateChoiceWithSharedAs d options container targetFun sel = editChoiceWithSharedAs d options container targetFun (Just sel)

editSharedChoice :: !d ![ChoiceOption a] ![a] (Shared (Maybe a)) -> Task a | descr d & iTask a
editSharedChoice d [] container sharedSel = editSharedChoiceSimple d container sharedSel
editSharedChoice d options container sharedSel = editSharedChoiceAs d options container id sharedSel

editSharedChoiceWithShared :: !d ![ChoiceOption a] !(ReadWriteShared [a] w) (Shared (Maybe a)) -> Task a | descr d & iTask a & iTask w
editSharedChoiceWithShared d [] sharedContainer sharedSel = editSharedChoiceWithSharedSimple d sharedContainer sharedSel
editSharedChoiceWithShared d options sharedContainer sharedSel = editSharedChoiceWithSharedAs d options sharedContainer id sharedSel

enterMultipleChoice :: !d ![MultiChoiceOption o] ![o] -> Task [o] | descr d & iTask o
enterMultipleChoice d options container
	= updateInformation d (multiChoiceToUpdate options) (container,[]) @ snd

updateMultipleChoice :: !d ![MultiChoiceOption o] ![o] [o] -> Task [o] | descr d & iTask o
updateMultipleChoice d options container sel
	= updateInformation d (multiChoiceToUpdate options) (container,sel) @ snd

enterSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) -> Task [o] | descr d & iTask o & iTask w
enterSharedMultipleChoice d options shared
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared []

updateSharedMultipleChoice :: !d ![MultiChoiceOption o] !(ReadWriteShared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w
updateSharedMultipleChoice d options shared sel 
	= updateInformationWithShared d (sharedMultiChoiceToUpdate options) shared sel

wait :: !d (r -> Bool) !(ReadWriteShared r w) -> Task r | descr d & iTask r
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
	=	viewInformation Void [] Void
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
viewTitle a = viewInformation (Title title) [ViewWith view] a <<@ InContainer //<<@ AfterLayout (tweakAttr titleFromValue)
where
	title = toSingleLineText a
	view a	= DivTag [] [SpanTag [StyleAttr "font-size: 30px"] [Text title]]

viewSharedTitle :: !(ReadWriteShared r w) -> Task r | iTask r
viewSharedTitle s = whileUnchanged s viewTitle
