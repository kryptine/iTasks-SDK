implementation module iTasks.UI.Layout.ReferenceImplementations

import StdList, StdTuple, StdBool, StdOverloaded, StdString, StdClass
import Data.Maybe
import Data.GenEq
import qualified Data.Map as DM

import iTasks.UI.Definition
import iTasks.UI.Layout

//Test if a specific UI at a path is in the selection
inUISelection_ :: UISelection UIPath UI -> Bool
inUISelection_ (SelectByPath p) path _ = p === path
inUISelection_ (SelectByDepth n) p _ = length p == n
inUISelection_ (SelectDescendents) [_:_] _ = True
inUISelection_ (SelectDescendents) _ _ = False
inUISelection_ (SelectByType t) _ (UI type _ _) = t === type
inUISelection_ (SelectByHasAttribute k) _ (UI _ attr _) = isJust ('DM'.get k attr)
inUISelection_ (SelectByAttribute k p) _ (UI _ attr _) = maybe False p ('DM'.get k attr)
inUISelection_ (SelectByNumChildren num) _ (UI _ _  items) = length items == num
inUISelection_ (SelectByContains selection) path ui=:(UI _ _ items)
	| inUISelection_ selection path ui = True 
			  						  = or [inUISelection_ (SelectByContains selection) (path ++ [i]) item \\ item <- items & i <- [0..]]
inUISelection_ (SelectRelative prefix sel) absolutePath ui 
	= maybe False (\relativePath -> inUISelection_ sel relativePath ui) (removePrefix prefix absolutePath)
where
	removePrefix [] psb = Just psb
	removePrefix [pa:psa] [pb:psb] = if (pa == pb) (removePrefix psa psb) Nothing
	removePrefix _ _ = Nothing
inUISelection_ (SelectNone) _ _ = False
inUISelection_ (SelectAND sell selr) path ui = inUISelection_ sell path ui && inUISelection_ selr path ui 
inUISelection_ (SelectOR sell selr) path ui = inUISelection_ sell path ui || inUISelection_ selr path ui 
inUISelection_ (SelectNOT sel) path ui = not (inUISelection_ sel path ui)

setUITypeRef_ :: UIType -> (UI -> UI)
setUITypeRef_ type = ref
where 
	ref (UI _ attr items) = UI type attr items

setUIAttributesRef_ :: UIAttributes -> (UI -> UI)
setUIAttributesRef_ extraAttr = ref
where
	ref (UI type attr items) = UI type ('DM'.union extraAttr attr) items

delUIAttributesRef_ :: UIAttributeSelection -> (UI -> UI)
delUIAttributesRef_ selection = ref
where
	ref (UI type attr items) = UI type (foldl (\a k -> if (matchKey_ selection k) ('DM'.del k a) a) attr ('DM'.keys attr)) items

modifyUIAttributesRef_ :: UIAttributeSelection (UIAttributes -> UIAttributes) -> (UI -> UI)
modifyUIAttributesRef_ selection modifier = ref
where
	ref (UI type attr items) = UI type ('DM'.union (modifier selected) attr) items
	where
		selected = selectAttributes selection attr

		selectAttributes SelectAll attr = attr
		selectAttributes (SelectKeys keys) attr = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

copySubUIAttributesRef_ :: UIAttributeSelection UIPath UIPath -> (UI -> UI)
copySubUIAttributesRef_ selection src dst = ref
where
	ref ui = updDst dst (selAttr src ui) ui

	selAttr [] (UI _ attr _) = [a \\ a=:(k,_) <- 'DM'.toList attr | matchKey_ selection k]
	selAttr [s:ss] (UI _ _ items) = if (s >= 0 && s < length items) (selAttr ss (items !! s)) []

	updDst [] selected (UI type attr items) = UI type (foldl (\a (k,v) -> 'DM'.put k v a) attr selected) items
	updDst [s:ss] selected ui=:(UI type attr items) = if (s >= 0 && s < length items) (UI type attr (updateAt s (updDst ss selected (items !! s)) items)) ui

wrapUIRef_ :: UIType -> (UI -> UI)
wrapUIRef_ type = ref
where
	ref ui = uic type [ui]

unwrapUIRef_ :: (UI -> UI)
unwrapUIRef_ = ref
where
	ref (UI _ _ [ui:_]) = ui
	ref ui = ui

insertChildUIRef_ :: Int UI -> (UI -> UI)
insertChildUIRef_ idx insert = ref
where
	ref ui=:(UI type attr items)
		| idx >= 0 && idx <= length items = UI type attr (insertAt idx insert items)
										  = ui

removeSubUIsRef_ :: UISelection -> (UI -> UI)
removeSubUIsRef_ selection = ref
where
	ref ui=:(UI type attr items) //Special case for the root node
	  | inUISelection_ selection [] ui = UI UIEmpty 'DM'.newMap []
							          = UI type attr (flatten [rem [i] x \\ x <- items & i <- [0..]])

	rem path ui=:(UI type attr items)
	  | inUISelection_ selection path ui = []
						      = [UI type attr (flatten [rem (path ++ [i]) x \\ x <- items & i <- [0..]])]

moveSubUIsRef_ :: UISelection UIPath Int -> (UI -> UI)
moveSubUIsRef_ selection dst pos = ref
where
	ref ui
		# (selected,Just ui`) = collect [] ui   //Find and remove all matching nodes
		# (dst`,pos`)   = adjust [] dst pos ui  //Adjust the path and position for the removals
		= (insert selected dst` pos` ui`)       //Insert the selected nodes at the destination

	collect path ui=:(UI type attr items)
		| not (startsWith path dst) && inUISelection_ selection path ui //Match
			= ([ui],Nothing)
		| otherwise //Check all children
			# (collects,items) = unzip [collect (path ++ [i]) x \\ x <- items & i <- [0..]]
			= (flatten collects, Just (UI type attr [x \\ Just x <- items]))

	startsWith [] dst = True
	startsWith [p:ps] [d:ds] = if (p == d) (startsWith ps ds) False
	startsWith _ _ = False

	adjust cur [] pos (UI _ _ items) = ([],index cur pos items)
	adjust cur [s:ss] pos (UI _ _ items)
		| s >= 0 && s < length items
			# (ss`,pos`) = adjust (cur ++ [s]) ss pos (items !! s)
			= ([index cur s items:ss`],pos`)
		| otherwise
			= ([s:ss],pos)

	index cur n items = length [x \\ x <- take n items & i <- [0..] | not (inUISelection_ selection (cur ++ [i]) x)]

	insert selected [] i (UI type attr items)
		| i >= 0 && i <= length items = UI type attr (take i items ++ selected  ++ drop i items)
									  = UI type attr items
	insert selected [s:ss] i (UI type attr items)
		| s >= 0 && s < length items
			= UI type attr (updateAt s (insert selected ss i (items !! s)) items)
			= UI type attr items

layoutSubUIsRef_ :: UISelection LayoutRule -> (UI -> UI)
layoutSubUIsRef_ selection layout = ref
where
	ref ui = app [] ui
	app path ui=:(UI type attr items) 
		| inUISelection_ selection path ui = applyLayoutRule layout ui
										  = UI type attr [app (path ++ [i]) x \\ x <- items & i <- [0..]]

sequenceLayoutsRef_ :: LayoutRule LayoutRule -> (UI -> UI)
sequenceLayoutsRef_ layout1 layout2 = ref
where
	ref ui = applyLayoutRule layout2 (applyLayoutRule layout1 ui)

applyLayoutRule :: LayoutRule UI -> UI 
applyLayoutRule rule ui = fst (extractUIWithEffects (rule (LUINo [0]) (initLUI ui,initLUIMoves)))
