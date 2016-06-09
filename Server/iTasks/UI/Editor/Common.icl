implementation module iTasks.UI.Editor.Common

import StdBool
import iTasks.UI.Definition, iTasks.UI.Editor
import Data.Tuple, Data.Error, Text, Text.JSON
import qualified Data.Map as DM

emptyEditor :: Editor a
emptyEditor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI _ _ vst			    = (Ok (ui UIEmpty,newFieldMask),vst)
	onEdit _ _ val mask vst 	= (Ok (NoChange,mask),val,vst)
	onRefresh _ _ val mask vst  = (Ok (NoChange,mask),val,vst)

listEditor :: (Maybe ([a] -> a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a]
listEditor add remove reorder count itemEditor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|taskId} = case genChildUIs dp 0 val [] vst of
		(Ok (items,masks),vst)
			//Add list structure editing buttons
			# items = [listItemControl (length val) idx dx \\ dx <- items & idx <- [0..]]
			//Add the add button
			# items = if (add =: Just _) (items ++ [addItemControl val]) items
			= (Ok (uic UIContainer items,CompoundMask masks), vst)
		(Error e,vst)  = (Error e,vst)
	where			
		genChildUIs dp _ [] us vst = (Ok (unzip (reverse us)), vst)
		genChildUIs dp i [c:cs] us vst = case itemEditor.Editor.genUI (dp++[i]) c vst of
			(Ok (u,m),vst) = genChildUIs dp (i+1) cs [(u,m):us] vst
			(Error e,vst)  = (Error e,vst)

		addItemControl val
			# counter  	= maybe [] (\f -> [uia UIViewString ('DM'.unions [widthAttr FlexSize, valueAttr (JSONString (f val))])]) count
			# button	= if (isJust add) [uia UIEditButton ('DM'.unions [iconClsAttr "icon-add",editAttrs taskId (editorId dp) (Just (JSONString "add"))])] []
			# attr      = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIContainer attr (counter ++ button)

		listItemControl numItems idx item
			# buttons	= (if reorder
							  [uia UIEditButton ('DM'.unions [iconClsAttr "icon-up", enabledAttr (idx <> 0), editAttrs taskId (editorId dp) (Just (JSONString ("mup_" +++ toString idx)))])
							  ,uia UIEditButton ('DM'.unions [iconClsAttr "icon-down", enabledAttr (idx <> numItems - 1), editAttrs taskId (editorId dp) (Just (JSONString ("mdn_" +++ toString idx)))])
							  ] []) ++
							  (if remove
							  [uia UIEditButton ('DM'.unions [iconClsAttr "icon-remove",editAttrs taskId (editorId dp) (Just (JSONString ("rem_" +++ toString idx)))])
							  ] [])
			# attr = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIContainer attr (if (reorder || remove) ([item] ++ buttons) [item])
			
	onEdit dp (tp,e) items listMask ust
		# childMasks = subMasks (length items) listMask
		# (items,childMasks,ust) = updateItems dp e items childMasks ust
		| isEmpty tp
			//Process the reordering commands
			# split = split "_" (fromMaybe "" (fromJSON e))
			# index = toInt (last split)
			# (items,childMasks) = case hd split of	
				"mup" = if reorder (swap items index,swap childMasks index) (items,childMasks)
				"mdn" = if reorder (swap items (index+1),swap childMasks (index+1)) (items,childMasks)
				"rem" = if remove  (removeAt index items,removeAt index childMasks)	(items,childMasks)
				"add" = case add of
					(Just f) = (insertAt (length items) (f items) items, insertAt (length items) (newFieldMask) childMasks)
					_        = (items,childMasks)
				_	
					= (items,childMasks)
			= (Ok (NoChange,CompoundMask childMasks),items,ust)
		| otherwise
			= (Ok (NoChange,CompoundMask childMasks),items,ust)
	where
		updateItems [i:tp] e items masks ust
			| i >= (length items) = (items,masks,ust)
			# (nm,nx,ust)	= itemEditor.Editor.onEdit dp (tp,e) (items !! i) (masks !! i) ust
			= case nm of
				Ok (_,m) = (updateAt i nx items, updateAt i m masks,ust)
				_    = (items,masks,ust)
		updateItems tp e items masks ust
			= (items,masks,ust)

		swap []	  _		= []
		swap list index
			| index == 0 			= list //prevent move first element up
			| index >= length list 	= list //prevent move last element down
			| otherwise				
				# f = list !! (index-1)
				# l = list !! (index)
				= updateAt (index-1) l (updateAt index f list)

	onRefresh dp new old mask vst = case genUI dp new vst of
		(Ok (ui,mask),vst) = (Ok (ReplaceUI ui,mask),new,vst)
		(Error e,vst) = (Error e,old,vst)


