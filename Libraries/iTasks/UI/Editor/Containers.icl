implementation module iTasks.UI.Editor.Containers
/**
* Editor combinators for the builtin containers
*
* To keep everything well-typed there are lots of boiler-plate versions to create the containers
*/
import iTasks.UI.Definition
import iTasks.UI.Editor
import Data.Error, Data.Func, Data.Functor, Data.Maybe, Data.Either
import Text.GenJSON
from Data.Map import :: Map

import StdBool, StdList, StdTuple, StdFunc

//Empty container
group :: !UIType -> Editor () ()
group type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr _ _ vst   = (Ok (uia type attr, CompoundState JSONNull []),vst)
	onEdit _ _ st vst    = (Ok (NoChange,st, Nothing),vst)
	onRefresh _ _ st vst = (Ok (NoChange, st, Nothing),vst)
	valueFromState _     = Just ()

groupl :: !UIType !(Editor a w) -> Editor [a] [(Int,w)]
groupl type {Editor|genUI=genUI_a,onEdit=onEdit_a,onRefresh=onRefresh_a,valueFromState=valueFromState_a}
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case editModeValue mode of
		Nothing = (Ok (UI type attr [], viewMode, []), vst)
		Just val = case genUIAll viewMode dp 0 val vst of
			(Error e,vst)            = (Error e,vst)
			(Ok (uis, childSts),vst) = (Ok (UI type attr uis, viewMode, childSts),vst)
	where
		viewMode = mode =: View _

	genUIAll _ _ _ [] vst = (Ok ([],[]),vst)
	genUIAll viewMode dp i [v:vs] vst = case genUI_a emptyAttr (dp ++ [i]) (if viewMode View Update $ v) vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui, st), vst) = case genUIAll viewMode dp (i + 1) vs vst of
			(Error e,      vst) = (Error e,                  vst)
			(Ok (uis, sts),vst) = (Ok ([ui: uis], [st: sts]),vst)

	onEdit dp ([i:tp],e) viewMode childSts vst
		| i < 0 || i >= length childSts  = (Error "Event route out of range",vst)
		| otherwise = case onEdit_a (dp ++ [i]) (tp,e) (childSts !! i) vst of
			(Error e,vst) = (Error e,vst)
			(Ok (NoChange, ist, mbw),vst)
				= (Ok (NoChange, viewMode, updateAt i ist childSts, fmap (\w -> [(i,w)]) mbw),vst)
			(Ok (change, ist, mbw),vst)
				= (Ok (ChangeUI [] [(i,ChangeChild change)], viewMode, updateAt i ist childSts, fmap (\w -> [(i,w)]) mbw),vst)

	onRefresh dp new viewMode childSts vst = case onRefreshAll dp 0 new childSts vst of
		(Error e, vst)                   = (Error e,vst)
		(Ok ([], childSts,ws), vst)      = (Ok (NoChange, viewMode, childSts, if (ws =:[]) Nothing (Just ws)),vst)
        (Ok (changes, childSts,ws), vst) = (Ok (ChangeUI [] changes, viewMode, childSts, if (ws =:[]) Nothing (Just ws)),vst)
	where
		onRefreshAll dp i [n: ns] [st: sts] vst
			 = case onRefresh_a (dp ++ [i]) n st vst of
				(Error e, vst) = (Error e, vst)
				(Ok (c, st, mbw),vst) = case onRefreshAll dp (i + 1) ns sts vst of
					(Error e,        vst) = (Error e, vst)
					(Ok (cs, sts,ws),vst) = (Ok ([(i,ChangeChild c):cs],[st: sts],ws ++ maybe [] (\w -> [(i,w)]) mbw), vst)

		onRefreshAll dp i ns [] vst //There are new elements in the list
			= case genUIAll viewMode dp i ns vst of
				(Error e,vst)      = (Error e,vst)
				(Ok (us, sts),vst) = (Ok ([(n,InsertChild u) \\ u <- us & n <- [i..]], sts,[]),vst)

		onRefreshAll dp i [] sts vst //Elements have been removed from the list
			= (Ok (repeatn (length sts) (i,RemoveChild),[],[]),vst)

	valueFromState _ childSts = valuesFromState childSts []
	where
		valuesFromState [] acc = Just $ reverse acc
		valuesFromState [st: sts] acc = case valueFromState_a st of
			Just val = valuesFromState sts [val: acc]
			_        = Nothing

groupL :: !UIType ![Editor a w] -> Editor [a] [(Int,w)]
groupL type editors = compoundEditorToEditor
	{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case editModeValue mode of
		Nothing = (Ok (UI type attr [], mode =: View _, []), vst)
		Just val = case genUIAll (mode =: View _) 0 editors dp val vst of
			(Error e,vst)            = (Error e,vst)
			(Ok (uis, childSts),vst) = (Ok (UI type attr uis, mode =: View _, childSts),vst)

	genUIAll viewMode i [ed:eds] dp [v:vs] vst
		= case ed.Editor.genUI emptyAttr (dp ++ [i]) (if viewMode View Update $ v) vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui,st),vst) = case genUIAll viewMode (i + 1) eds dp vs vst of
				(Error e,vst) = (Error e,vst)
				(Ok (uis, sts),vst) = (Ok ([ui:uis],[st: sts]),vst)
	genUIAll viewMode _ _ _ _ vst = (Ok ([], []), vst)

	onEdit dp ([i:tp],e) viewMode childSts vst
		| i < 0 || i >= length childSts  = (Error "Event route out of range",vst)
		= case (editors !! i).Editor.onEdit (dp ++ [i]) (tp,e) (childSts !! i) vst of
			(Error e,vst) = (Error e,vst)
			(Ok (NoChange,ist,mbw),vst)
				= (Ok (NoChange,viewMode,updateAt i ist childSts,fmap (\w -> [(i,w)]) mbw),vst)
			(Ok (change,ist,mbw),vst)
				= (Ok (ChangeUI [] [(i,ChangeChild change)],viewMode,updateAt i ist childSts,fmap (\w -> [(i,w)]) mbw), vst)

	onRefresh dp new viewMode childSts vst = case onRefreshAll 0 editors dp new childSts vst of
		(Error e, vst)                 = (Error e, vst)
		(Ok ([],childSts,ws),vst)      = (Ok (NoChange,viewMode,childSts,if (ws =:[]) Nothing (Just ws)),vst)
		(Ok (changes,childSts,ws),vst) = (Ok (ChangeUI [] changes,viewMode,childSts,if (ws =:[]) Nothing (Just ws)),vst)
	where
		onRefreshAll i [ed:eds] dp [n:ns] [st:sts] vst
			 = case ed.Editor.onRefresh (dp ++ [i]) n st vst of
				(Error e,vst) = (Error e,vst)
				(Ok (c,st,mbw),vst) = case onRefreshAll (i + 1) eds dp ns sts vst of
					(Error e,vst)        = (Error e,vst)
					(Ok (cs,sts,ws),vst) = (Ok ([(i,ChangeChild c):cs],[st:sts],ws ++ maybe [] (\w -> [(i,w)]) mbw),vst)

		//There are new elements in the list
		onRefreshAll i editors dp ns [] vst
			= case genUIAll viewMode i editors dp ns vst of
				(Error e,vst)     = (Error e,vst)
				(Ok (us,sts),vst) = (Ok ([(n,InsertChild u) \\ u <- us & n <- [i..]],sts, []),vst)

		//Elements have been removed from the list
		onRefreshAll i _ dp [] sts vst
			= (Ok (repeatn (length sts) (i,RemoveChild),[],[]),vst)

		//There are not enough editors
		onRefreshAll _ _ _ _ _ vst = (Ok ([], [], []), vst)

	valueFromState _ childSts = valuesFromState childSts editors []
	where
		valuesFromState [st: sts] [editor: editors] acc = case editor.Editor.valueFromState st of
			Just val = valuesFromState sts editors [val: acc]
			_        = Nothing
		valuesFromState _ _ acc = Just $ reverse acc

group1 :: !UIType !(Editor a w) -> Editor a w
group1 type {Editor |genUI=genUI_a,onEdit=onEdit_a,onRefresh=onRefresh_a,valueFromState=valueFromState_a}
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case genUI_a emptyAttr (dp ++ [0]) (mapEditMode id mode) vst of
		(Error e,vst)        = (Error e,vst)
		(Ok (ui1,mask1),vst) = (Ok (UI type attr [ui1], (), [mask1]),vst)

	onEdit dp ([0:tp],e) _ [m1] vst = case onEdit_a (dp ++ [0]) (tp,e) m1 vst of
		(Error e,vst)              = (Error e,vst)
		(Ok (NoChange,m1,mbw),vst) = (Ok (NoChange, (), [m1],mbw),vst)
		(Ok (c1,m1,mbw),vst)       = (Ok (ChangeUI [] [(0,ChangeChild c1)], (), [m1],mbw),vst)
	onEdit _ _ _ _ vst = (Error "Event route out of range",vst)

	onRefresh dp new _ [m1] vst = case onRefresh_a (dp ++ [0]) new m1 vst of
		(Error e,vst)              = (Error e,vst)
		(Ok (NoChange,m1,mbw),vst) = (Ok (NoChange, (), [m1], mbw),vst)
		(Ok (c1,m1,mbw),vst)       = (Ok (ChangeUI [] [(0,ChangeChild c1)], (), [m1], mbw),vst)

	valueFromState _ [m1] = case valueFromState_a m1 of
		Just val1 = Just val1
		_         = Nothing
	valueFromState _ _ = Nothing

group2 :: !UIType !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
group2 type {Editor |genUI=genUI_a,onEdit=onEdit_a,onRefresh=onRefresh_a,valueFromState=valueFromState_a}
            {Editor |genUI=genUI_b,onEdit=onEdit_b,onRefresh=onRefresh_b,valueFromState=valueFromState_b}
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case genUI_a emptyAttr (dp ++ [0]) (mapEditMode fst mode) vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui1,m1),vst) = case genUI_b emptyAttr (dp ++ [1]) (mapEditMode snd mode) vst of
			(Error e,vst)     = (Error e,vst)
			(Ok (ui2,m2),vst) = (Ok (UI type attr [ui1,ui2], (), [m1,m2]),vst)

	onEdit dp ([0:tp],e) _ [m1,m2] vst = case onEdit_a (dp ++ [0]) (tp,e) m1 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m1,mbw1),vst) = (Ok (NoChange, (), [m1,m2], fmap (\w -> (Just w,Nothing)) mbw1),vst)
		(Ok (c1,m1,mbw1),vst)       = (Ok (ChangeUI [] [(0,ChangeChild c1)], (), [m1,m2], fmap (\w -> (Just w,Nothing)) mbw1),vst)

	onEdit dp ([1:tp],e) _ [m1,m2] vst = case onEdit_b (dp ++ [1]) (tp,e) m2 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m2,mbw2),vst) = (Ok (NoChange, (), [m1,m2], fmap (\w -> (Nothing,Just w)) mbw2),vst)
		(Ok (c2,m2,mbw2),vst)       = (Ok (ChangeUI [] [(1,ChangeChild c2)], (), [m1,m2], fmap (\w -> (Nothing,Just w)) mbw2),vst)
	onEdit _ _ _ _ vst              = (Error "Event route out of range",vst)
	
	onRefresh dp (n1,n2) _ [m1,m2] vst
		= case onRefresh_a (dp ++ [0]) n1 m1 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (c1,m1,mbw1),vst) = case onRefresh_b (dp ++ [1]) n2 m2 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (c2,m2,mbw2),vst)
					# changes = [(0,ChangeChild c1),(1,ChangeChild c2)]
					# change = case changes of
						[] = NoChange
						_  = ChangeUI [] changes
					= (Ok (change, (), [m1,m2],if (mbw1=:(Just _) || mbw2=:(Just _)) (Just (mbw1,mbw2)) Nothing),vst)

	valueFromState _ [m1, m2] = case (valueFromState_a m1, valueFromState_b m2) of
		(Just val1, Just val2) = Just (val1, val2)
		_                      = Nothing


group3 :: !UIType !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
group3 type {Editor |genUI=genUI_a,onEdit=onEdit_a,onRefresh=onRefresh_a,valueFromState=valueFromState_a}
            {Editor |genUI=genUI_b,onEdit=onEdit_b,onRefresh=onRefresh_b,valueFromState=valueFromState_b}
            {Editor |genUI=genUI_c,onEdit=onEdit_c,onRefresh=onRefresh_c,valueFromState=valueFromState_c}
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case genUI_a emptyAttr (dp ++ [0]) (mapEditMode fst3 mode) vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui1,m1),vst) = case genUI_b emptyAttr (dp ++ [1]) (mapEditMode snd3 mode) vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui2,m2),vst) = case genUI_c emptyAttr (dp ++ [2]) (mapEditMode thd3 mode) vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui3,m3),vst) =(Ok (UI type attr [ui1,ui2,ui3], (), [m1,m2,m3]),vst)

	onEdit dp ([0:tp],e) _ [m1,m2,m3] vst = case onEdit_a (dp ++ [0]) (tp,e) m1 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m1,mbw1),vst) = (Ok (NoChange, (), [m1,m2,m3], fmap (\w -> (Just w,Nothing,Nothing)) mbw1),vst)
		(Ok (c1,m1,mbw1),vst)       = (Ok (ChangeUI [] [(0,ChangeChild c1)], (), [m1,m2,m3], Nothing),vst)

	onEdit dp ([1:tp],e) _ [m1,m2,m3] vst = case onEdit_b (dp ++ [1]) (tp,e) m2 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m2,mbw2),vst) = (Ok (NoChange, (), [m1,m2,m3], Nothing),vst)
		(Ok (c2,m2,mbw2),vst)       = (Ok (ChangeUI [] [(1,ChangeChild c2)], (), [m1,m2,m3], fmap (\w -> (Nothing,Just w,Nothing)) mbw2),vst)

	onEdit dp ([2:tp],e) _ [m1,m2,m3] vst = case onEdit_c (dp ++ [2]) (tp,e) m3 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m3,mbw3),vst) = (Ok (NoChange, (), [m1,m2,m3], Nothing),vst)
		(Ok (c3,m3,mbw3),vst)       = (Ok (ChangeUI [] [(2,ChangeChild c3)], (), [m1,m2,m3], fmap (\w -> (Nothing,Nothing,Just w)) mbw3),vst)

	onEdit _ _ _ _ vst = (Error "Event route out of range",vst)
	
	onRefresh dp (n1,n2,n3) _ [m1,m2,m3] vst = case onRefresh_a (dp ++ [0]) n1 m1 vst of
		(Error e,vst) = (Error e,vst)
		(Ok (c1,m1,mbw1),vst) = case onRefresh_b (dp ++ [1]) n2 m2 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (c2,m2,mbw2),vst) = case onRefresh_c (dp ++ [2]) n3 m3 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (c3,m3,mbw3),vst)
					# changes = [(0,ChangeChild c1),(1,ChangeChild c2),(2,ChangeChild c3)]
					# change = case changes of
						[] = NoChange
						_  = ChangeUI [] changes
					= (Ok (change, (), [m1,m2,m3], if (mbw1=:(Just _) || mbw2=:(Just _) || mbw3=:(Just _)) (Just (mbw1,mbw2,mbw3)) Nothing),vst)

	valueFromState _ [m1, m2, m3] = case (valueFromState_a m1, valueFromState_b m2, valueFromState_c m3) of
		(Just val1, Just val2, Just val3) = Just (val1, val2, val3)
		_                                 = Nothing

group4 :: !UIType !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
group4 type {Editor |genUI=genUI_a,onEdit=onEdit_a,onRefresh=onRefresh_a,valueFromState=valueFromState_a}
            {Editor |genUI=genUI_b,onEdit=onEdit_b,onRefresh=onRefresh_b,valueFromState=valueFromState_b}
            {Editor |genUI=genUI_c,onEdit=onEdit_c,onRefresh=onRefresh_c,valueFromState=valueFromState_c}
            {Editor |genUI=genUI_d,onEdit=onEdit_d,onRefresh=onRefresh_d,valueFromState=valueFromState_d}
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case genUI_a emptyAttr (dp ++ [0]) (mapEditMode (\(a, _, _, _) -> a) mode) vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui1,m1),vst) = case genUI_b emptyAttr (dp ++ [1]) (mapEditMode (\(_, b, _, _) -> b) mode) vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui2,m2),vst) = case genUI_c emptyAttr (dp ++ [2]) (mapEditMode (\(_, _, c, _) -> c) mode) vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui3,m3),vst) = case genUI_d emptyAttr (dp ++ [3]) (mapEditMode (\(_, _, _, d) -> d) mode) vst of
					(Error e,vst) = (Error e,vst)
					(Ok (ui4,m4),vst) = (Ok (UI type attr [ui1,ui2,ui3,ui4], (), [m1,m2,m3,m4]),vst)

	onEdit dp ([0:tp],e) _ [m1,m2,m3,m4] vst = case onEdit_a (dp ++ [0]) (tp,e) m1 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m1,mbw1),vst) = (Ok (NoChange, (), [m1,m2,m3,m4], fmap (\w -> (Just w,Nothing,Nothing,Nothing)) mbw1),vst)
		(Ok (c1,m1,mbw1),vst)       = (Ok (ChangeUI [] [(0,ChangeChild c1)], (), [m1,m2,m3,m4], fmap (\w -> (Just w,Nothing,Nothing,Nothing)) mbw1),vst)

	onEdit dp ([1:tp],e) _ [m1,m2,m3,m4] vst = case onEdit_b (dp ++ [1]) (tp,e) m2 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m2,mbw2),vst) = (Ok (NoChange, (), [m1,m2,m3,m4], fmap (\w -> (Nothing,Just w,Nothing,Nothing)) mbw2),vst)
		(Ok (c2,m2,mbw2),vst)       = (Ok (ChangeUI [] [(1,ChangeChild c2)], (), [m1,m2,m3,m4], fmap (\w -> (Nothing,Just w,Nothing,Nothing)) mbw2),vst)

	onEdit dp ([2:tp],e) _ [m1,m2,m3,m4] vst = case onEdit_c (dp ++ [2]) (tp,e) m3 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m3,mbw3),vst) = (Ok (NoChange, (), [m1,m2,m3,m4], fmap (\w -> (Nothing,Nothing,Just w,Nothing)) mbw3),vst)
		(Ok (c3,m3,mbw3),vst)       = (Ok (ChangeUI [] [(2,ChangeChild c3)], (), [m1,m2,m3,m4], fmap (\w -> (Nothing,Nothing,Just w,Nothing)) mbw3),vst)

	onEdit dp ([3:tp],e) _ [m1,m2,m3,m4] vst = case onEdit_d (dp ++ [3]) (tp,e) m4 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m4,mbw4),vst) = (Ok (NoChange, (), [m1,m2,m3,m4], fmap (\w -> (Nothing,Nothing,Nothing,Just w)) mbw4),vst)
		(Ok (c4,m4,mbw4),vst)       = (Ok (ChangeUI [] [(3,ChangeChild c4)], (), [m1,m2,m3,m4], fmap (\w -> (Nothing,Nothing,Nothing,Just w)) mbw4),vst)

	onEdit _ _ _ _ vst = (Error "Event route out of range",vst)

	onRefresh dp (n1,n2,n3,n4) _ [m1,m2,m3,m4] vst = case onRefresh_a (dp ++ [0]) n1 m1 vst of
		(Error e,vst) = (Error e,vst)
		(Ok (c1,m1,mbw1),vst) = case onRefresh_b (dp ++ [1]) n2 m2 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (c2,m2,mbw2),vst) = case onRefresh_c (dp ++ [2]) n3 m3 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (c3,m3,mbw3),vst) = case onRefresh_d (dp ++ [3]) n4 m4 vst of
					(Error e,vst) = (Error e,vst)
					(Ok (c4,m4,mbw4),vst)
						# changes = [(0,ChangeChild c1),(1,ChangeChild c2),(2,ChangeChild c3),(3,ChangeChild c4)]
						# change = case changes of
							[] = NoChange
							_  = ChangeUI [] changes
						= (Ok (change, (), [m1,m2,m3,m4],
							if (mbw1=:(Just _) || mbw2=:(Just _) || mbw3=:(Just _) || mbw4=:(Just _)) (Just (mbw1,mbw2,mbw3,mbw4)) Nothing),vst)

	valueFromState _ [m1, m2, m3, m4] =
		case (valueFromState_a m1, valueFromState_b m2, valueFromState_c m3, valueFromState_d m4) of
			(Just val1, Just val2, Just val3, Just val4) = Just (val1, val2, val3, val4)
			_                      = Nothing

group5 :: !UIType !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
group5 type {Editor |genUI=genUI_a,onEdit=onEdit_a,onRefresh=onRefresh_a,valueFromState=valueFromState_a}
            {Editor |genUI=genUI_b,onEdit=onEdit_b,onRefresh=onRefresh_b,valueFromState=valueFromState_b}
            {Editor |genUI=genUI_c,onEdit=onEdit_c,onRefresh=onRefresh_c,valueFromState=valueFromState_c}
            {Editor |genUI=genUI_d,onEdit=onEdit_d,onRefresh=onRefresh_d,valueFromState=valueFromState_d}
            {Editor |genUI=genUI_e,onEdit=onEdit_e,onRefresh=onRefresh_e,valueFromState=valueFromState_e}
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case genUI_a emptyAttr (dp ++ [0]) (mapEditMode (\(a, _, _, _, _) -> a) mode) vst of
		(Error e,vst) = (Error e,vst)
		(Ok (ui1,m1),vst) = case genUI_b emptyAttr (dp ++ [1]) (mapEditMode (\(_, b, _, _, _) -> b) mode) vst of
			(Error e,vst) = (Error e,vst)
			(Ok (ui2,m2),vst) = case genUI_c emptyAttr (dp ++ [2]) (mapEditMode (\(_, _, c, _, _) -> c) mode) vst of
				(Error e,vst) = (Error e,vst)
				(Ok (ui3,m3),vst) = case genUI_d emptyAttr (dp ++ [3]) (mapEditMode (\(_, _, _, d, _) -> d) mode) vst of
					(Error e,vst) = (Error e,vst)
					(Ok (ui4,m4),vst) = case genUI_e emptyAttr (dp ++ [4]) (mapEditMode (\(_, _, _, _, e) -> e) mode) vst of
						(Error e,vst) = (Error e,vst)
						(Ok (ui5,m5),vst) = (Ok (UI type attr [ui1,ui2,ui3,ui4,ui5], (), [m1,m2,m3,m4,m5]),vst)

	onEdit dp ([0:tp],e) _ [m1,m2,m3,m4,m5] vst = case onEdit_a (dp ++ [0]) (tp,e) m1 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m1,mbw1),vst) = (Ok (NoChange, (), [m1,m2,m3,m4,m5],fmap (\w -> (Just w,Nothing,Nothing,Nothing,Nothing)) mbw1),vst)
		(Ok (c1,m1,mbw1),vst)       = (Ok (ChangeUI [] [(0,ChangeChild c1)], (), [m1,m2,m3,m4,m5],fmap (\w -> (Just w,Nothing,Nothing,Nothing,Nothing)) mbw1),vst)

	onEdit dp ([1:tp],e) _ [m1,m2,m3,m4,m5] vst = case onEdit_b (dp ++ [1]) (tp,e) m2 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m2,mbw2),vst) = (Ok (NoChange, (), [m1,m2,m3,m4,m5],fmap (\w -> (Nothing,Just w,Nothing,Nothing,Nothing)) mbw2),vst)
		(Ok (c2,m2,mbw2),vst)       = (Ok (ChangeUI [] [(1,ChangeChild c2)], (), [m1,m2,m3,m4,m5],fmap (\w -> (Nothing,Just w,Nothing,Nothing,Nothing)) mbw2),vst)

	onEdit dp ([2:tp],e) _ [m1,m2,m3,m4,m5] vst = case onEdit_c (dp ++ [2]) (tp,e) m3 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m3,mbw3),vst) = (Ok (NoChange, (), [m1,m2,m3,m4,m5],fmap (\w -> (Nothing,Nothing,Just w,Nothing,Nothing)) mbw3),vst)
		(Ok (c3,m3,mbw3),vst)       = (Ok (ChangeUI [] [(2,ChangeChild c3)], (), [m1,m2,m3,m4,m5],fmap (\w -> (Nothing,Nothing,Just w,Nothing,Nothing)) mbw3),vst)

	onEdit dp ([3:tp],e) _ [m1,m2,m3,m4,m5] vst = case onEdit_d (dp ++ [3]) (tp,e) m4 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m4,mbw4),vst) = (Ok (NoChange, (), [m1,m2,m3,m4,m5], fmap (\w -> (Nothing,Nothing,Nothing,Just w,Nothing)) mbw4),vst)
		(Ok (c4,m4,mbw4),vst)       = (Ok (ChangeUI [] [(3,ChangeChild c4)], (), [m1,m2,m3,m4,m5], fmap (\w -> (Nothing,Nothing,Nothing,Just w,Nothing)) mbw4),vst)

	onEdit dp ([4:tp],e) _ [m1,m2,m3,m4,m5] vst = case onEdit_e (dp ++ [4]) (tp,e) m5 vst of
		(Error e,vst)               = (Error e,vst)
		(Ok (NoChange,m5,mbw5),vst) = (Ok (NoChange, (), [m1,m2,m3,m4,m5], fmap (\w -> (Nothing,Nothing,Nothing,Nothing,Just w)) mbw5),vst)
		(Ok (c5,m5,mbw5),vst)       = (Ok (ChangeUI [] [(4,ChangeChild c5)], (), [m1,m2,m3,m4,m5], fmap (\w -> (Nothing,Nothing,Nothing,Nothing,Just w)) mbw5),vst)

	onEdit _ _ _ _ vst = (Error "Event route out of range",vst)

	onRefresh dp (n1,n2,n3,n4,n5) _ [m1,m2,m3,m4,m5] vst = case onRefresh_a (dp ++ [0]) n1 m1 vst of
		(Error e,vst) = (Error e,vst)
		(Ok (c1,m1,mbw1),vst) = case onRefresh_b (dp ++ [1]) n2 m2 vst of
			(Error e,vst) = (Error e,vst)
			(Ok (c2,m2,mbw2),vst) = case onRefresh_c (dp ++ [2]) n3 m3 vst of
				(Error e,vst) = (Error e,vst)
				(Ok (c3,m3,mbw3),vst) = case onRefresh_d (dp ++ [3]) n4 m4 vst of
					(Error e,vst) = (Error e,vst)
					(Ok (c4,m4,mbw4),vst) = case onRefresh_e (dp ++ [4]) n5 m5 vst of
						(Error e,vst) = (Error e,vst)
						(Ok (c5,m5,mbw5),vst)
							# changes = [(0,ChangeChild c1),(1,ChangeChild c2),(2,ChangeChild c3),(3,ChangeChild c4),(4,ChangeChild c5)]
							# change = case changes of
								[] = NoChange
								_  = ChangeUI [] changes
							= (Ok (change, (), [m1,m2,m3,m4,m5],
								if (mbw1=:(Just _) || mbw2=:(Just _) || mbw3=:(Just _) || mbw4=:(Just _) || mbw5=:(Just _))
									(Just (mbw1,mbw2,mbw3,mbw4,mbw5)) Nothing),vst)

	valueFromState _ [m1, m2, m3, m4, m5] =
		case (valueFromState_a m1, valueFromState_b m2, valueFromState_c m3, valueFromState_d m4, valueFromState_e m5) of
			(Just val1, Just val2, Just val3, Just val4, Just val5) = Just (val1, val2, val3, val4, val5)
			_                                                       = Nothing

groupc :: !UIType !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int, a) (Either Int w)
groupc type {Editor|genUI=choiceEditorGenUI,onEdit=choiceEditorOnEdit,onRefresh=choiceEditorOnRefresh,valueFromState=choiceEditorValueFromState} fieldEditors = compoundEditorToEditor
	{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI attr dp mode vst = case choiceEditorGenUI emptyAttr (dp ++ [0]) (mapEditMode fst mode) vst of
		(Error e,vst) = (Error e,vst)
		(Ok (uiSelector, stSelector),vst) = case choiceEditorValueFromState stSelector of
			//Only generate the field UI if no selection has been made
			Nothing = (Ok (UI type attr [uiSelector], mode =: View _, [stSelector]),vst)
			Just choice = case fieldEditors !? choice of
				//Only generate the field UI if selection is out of bounds
				Nothing = (Ok (UI type attr [uiSelector], mode =: View _, [stSelector]),vst)
				Just (selectFun, editor)
					= case editor.Editor.genUI emptyAttr (dp ++ [1]) (mapEditMode (selectFun o Just o snd) mode) vst of
						(Error e,vst) = (Error e,vst)
						(Ok (uiField, stField),vst)
							 = (Ok (UI type attr [uiSelector, uiField], mode =: View _, [stSelector, stField]),vst)
	
	//Handle choice changes
	onEdit dp ([0:tp],choiceEdit) viewMode st=:[stateSelector:optStateField] vst
		= case choiceEditorOnEdit (dp ++ [0]) (tp,choiceEdit) stateSelector vst of
			(Error e,vst) = (Error e,vst)
			(Ok (choiceUIChange,stateSelector,_),vst)
				# (Just currentChoice, val) = valAndChoiceFromState st
				# mbNewChoice = choiceEditorValueFromState stateSelector
				//Based on the effect of the selection change we may need to update the field editor
				= case (optStateField, mbNewChoice) of
					//Previously no choice was made, but now a choice (within the bounds) has been made
					//-> create an initial UI
					([], Just newChoice) | newChoice >= 0 && newChoice < length fieldEditors
						# (selectFun, editor) = fieldEditors !! newChoice
						# val = selectFun val
						= case editor.Editor.genUI emptyAttr (dp ++ [1]) (if viewMode View Update $ val) vst of
							(Error e,vst) = (Error e,vst)
							(Ok (uiField, stateField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,InsertChild uiField)]
								= (Ok (change,viewMode,[stateSelector, stateField], Just (Left newChoice)), vst)
					//Previously no choice was made and still no choice has been made
					([], _)
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange)]
						= (Ok (change,viewMode,[stateSelector], Nothing), vst)
					//A new choice (within the bounds) has been made
					(_, Just newChoice) | newChoice >= 0 && newChoice < length fieldEditors
						| newChoice == currentChoice //The selection stayed the same
							# change = ChangeUI [] [(0,ChangeChild choiceUIChange)]
							= (Ok (change,viewMode,[stateSelector:optStateField], Nothing), vst)
						| otherwise //The selection changed -> replace with an initial UI of the new choice
							# (selectFun,editor) = fieldEditors !! newChoice
							# val = selectFun val
							= case editor.Editor.genUI emptyAttr (dp ++ [1]) (if viewMode View Update $ val) vst of
								(Error e,vst) = (Error e,vst)
								(Ok (uiField,stateField),vst)
									# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,ChangeChild (ReplaceUI uiField))]
									= (Ok (change,viewMode,[stateSelector, stateField], Just (Left newChoice)), vst)
					//The selection has been cleared or an invalid choice is made
					_
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,RemoveChild)]
						= (Ok (change,viewMode,[stateSelector],Nothing), vst)

	//Handle edits in the field editor
	onEdit dp ([1:tp],fieldEdit) viewMode st=:[stateSelector, stateField] vst = case valAndChoiceFromState st of
		(Just choice, _) = case fieldEditors !? choice of
			Just (_, editor) = case editor.Editor.onEdit (dp ++ [1]) (tp,fieldEdit) stateField vst of
				(Error e,vst) = (Error e,vst)
				(Ok (fieldChange, stateField,mbw),vst)
					# change = ChangeUI [] [(1,ChangeChild fieldChange)]
					= (Ok (change,viewMode,[stateSelector, stateField], fmap Right mbw), vst)
			Nothing = (Ok (NoChange, viewMode, st, Nothing), vst)
		_ = (Ok (NoChange, viewMode, st, Nothing), vst)

	onEdit _ _ _ _ vst = (Error "Event route out of range",vst)

	onRefresh dp (newChoice, newField) viewMode st=:[stateSelector:optStateField] vst
		//Update the choice selector
		= case choiceEditorOnRefresh (dp ++ [0]) newChoice stateSelector vst of
			(Error e,vst) = (Error e,vst)
			(Ok (choiceUIChange,stateSelector,_),vst)
				| optStateField =:[] //Previously no choice was made
					//Still no choice has been made or choice is out of bounds
					| isNothing (choiceEditorValueFromState stateSelector) ||
					  newChoice < 0 || newChoice >= length fieldEditors
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange)]
						= (Ok (change,viewMode,[stateSelector],Nothing), vst)
					| otherwise //A choice has been made -> create an initial UI
						# (selectFun, editor) = fieldEditors !! newChoice
						# newField = selectFun $ Just newField
						= case editor.Editor.genUI emptyAttr (dp ++ [1]) (if viewMode View Update $ newField) vst of
							(Error e,vst) = (Error e,vst)
							(Ok (uiField,stateField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,InsertChild uiField)]
								= (Ok (change,viewMode,[stateSelector,stateField],Nothing), vst)
				| otherwise // Previously an editor was chosen
					# mbOldChoice = fst <$> valueFromState viewMode st
					//The selection has been cleared or the choice is out of bounds
					| isNothing (choiceEditorValueFromState stateSelector) ||
					  newChoice < 0 || newChoice >= length fieldEditors
						# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,RemoveChild)]
						= (Ok (change,viewMode,[stateSelector],Nothing), vst)
					| Just newChoice == mbOldChoice //The selection stayed the same -> update the field
						# (selectFun,editor) = fieldEditors !! newChoice
						= case editor.Editor.onRefresh (dp ++ [1]) newField (hd optStateField) vst of
							(Error e,vst) = (Error e,vst)
							(Ok (fieldUIChange,stateField,_),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,ChangeChild fieldUIChange)]
								= (Ok (change,viewMode,[stateSelector,stateField],Nothing), vst)
					| otherwise //The selection changed -> replace with an initial UI of the new choice	
						# (selectFun,editor) = fieldEditors !! newChoice
						# newField = selectFun $ Just newField
						= case editor.Editor.genUI emptyAttr (dp ++ [1]) (if viewMode View Update $ newField) vst of
							(Error e,vst) = (Error e,vst)
							(Ok (uiField,stateField),vst)
								# change = ChangeUI [] [(0,ChangeChild choiceUIChange),(1,ChangeChild (ReplaceUI uiField))]
								= (Ok (change,viewMode,[stateSelector,stateField],Nothing), vst)

	valueFromState _ st = case valAndChoiceFromState st of
		(Just choice, Just val) = Just (choice, val)
		_                       = Nothing

	valAndChoiceFromState [stateSelector, childSt] = case choiceEditorValueFromState stateSelector of
		Just choice = case fieldEditors !? choice of
			Just (_, editor) = case editor.Editor.valueFromState childSt of
				Just val = (Just choice, Just val)
				_        = (Just choice, Nothing)
			_ = (Nothing, Nothing)
		_ = (Nothing, Nothing)
	valAndChoiceFromState _ = (Nothing, Nothing)

//# UIContainer
container :: Editor () ()
container = group UIContainer

containerl :: !(Editor a w) -> Editor [a] [(Int,w)]
containerl e = groupl UIContainer e

containerL :: ![Editor a w] -> Editor [a] [(Int,w)]
containerL es = groupL UIContainer es

container1 :: !(Editor a w) -> Editor a w
container1 e1 = group1 UIContainer e1

container2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
container2 e1 e2 = group2 UIContainer e1 e2

container3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
container3 e1 e2 e3 = group3 UIContainer e1 e2 e3

container4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
container4 e1 e2 e3 e4 = group4 UIContainer e1 e2 e3 e4

container5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
container5 e1 e2 e3 e4 e5 = group5 UIContainer e1 e2 e3 e4 e5

containerc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
containerc ec es = groupc UIContainer ec es

//# UIPanel
panel :: Editor () ()
panel = group UIPanel

panell :: !(Editor a w) -> Editor [a] [(Int,w)]
panell e = groupl UIPanel e

panelL :: ![Editor a w] -> Editor [a] [(Int,w)]
panelL es = groupL UIPanel es

panel1 :: !(Editor a w) -> Editor a w
panel1 e1 = group1 UIPanel e1

panel2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
panel2 e1 e2 = group2 UIPanel e1 e2

panel3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
panel3 e1 e2 e3 = group3 UIPanel e1 e2 e3

panel4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
panel4 e1 e2 e3 e4 = group4 UIPanel e1 e2 e3 e4

panel5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
panel5 e1 e2 e3 e4 e5 = group5 UIPanel e1 e2 e3 e4 e5

panelc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
panelc ec es = groupc UIPanel ec es

//# UITabSet
tabset :: Editor () ()
tabset = group UITabSet

tabsetl :: !(Editor a w) -> Editor [a] [(Int,w)]
tabsetl e = groupl UITabSet e

tabsetL :: ![Editor a w] -> Editor [a] [(Int,w)]
tabsetL es = groupL UITabSet es

tabset1 :: !(Editor a w) -> Editor a w
tabset1 e1 = group1 UITabSet e1

tabset2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
tabset2 e1 e2 = group2 UITabSet e1 e2

tabset3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
tabset3 e1 e2 e3 = group3 UITabSet e1 e2 e3

tabset4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
tabset4 e1 e2 e3 e4 = group4 UITabSet e1 e2 e3 e4

tabset5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
tabset5 e1 e2 e3 e4 e5 = group5 UITabSet e1 e2 e3 e4 e5

tabsetc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
tabsetc ec es = groupc UITabSet ec es

//# UIWindow
window :: Editor () ()
window = group UIWindow

windowl :: !(Editor a w) -> Editor [a] [(Int,w)]
windowl e = groupl UIWindow e

windowL :: ![Editor a w] -> Editor [a] [(Int,w)]
windowL es = groupL UIWindow es

window1 :: !(Editor a w) -> Editor a w
window1 e1 = group1 UIWindow e1

window2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
window2 e1 e2 = group2 UIWindow e1 e2

window3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
window3 e1 e2 e3 = group3 UIWindow e1 e2 e3

window4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
window4 e1 e2 e3 e4 = group4 UIWindow e1 e2 e3 e4

window5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
window5 e1 e2 e3 e4 e5 = group5 UIWindow e1 e2 e3 e4 e5

windowc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
windowc ec es = groupc UIWindow ec es

//# UIMenu
menu :: Editor () ()
menu = group UIMenu

menul :: !(Editor a w) -> Editor [a] [(Int,w)]
menul e = groupl UIMenu e

menuL :: ![Editor a w] -> Editor [a] [(Int,w)]
menuL es = groupL UIMenu es

menu1 :: !(Editor a w) -> Editor a w
menu1 e1 = group1 UIMenu e1

menu2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
menu2 e1 e2 = group2 UIMenu e1 e2

menu3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
menu3 e1 e2 e3 = group3 UIMenu e1 e2 e3

menu4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
menu4 e1 e2 e3 e4 = group4 UIMenu e1 e2 e3 e4

menu5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
menu5 e1 e2 e3 e4 e5 = group5 UIMenu e1 e2 e3 e4 e5

menuc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
menuc ec es = groupc UIMenu ec es

//# UIToolBar
toolbar :: Editor () ()
toolbar = group UIToolBar

toolbarl :: !(Editor a w) -> Editor [a] [(Int,w)]
toolbarl e = groupl UIToolBar e

toolbarL :: ![Editor a w] -> Editor [a] [(Int,w)]
toolbarL es = groupL UIToolBar es

toolbar1 :: !(Editor a w) -> Editor a w
toolbar1 e1 = group1 UIToolBar e1

toolbar2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
toolbar2 e1 e2 = group2 UIToolBar e1 e2

toolbar3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
toolbar3 e1 e2 e3 = group3 UIToolBar e1 e2 e3

toolbar4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
toolbar4 e1 e2 e3 e4 = group4 UIToolBar e1 e2 e3 e4

toolbar5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
toolbar5 e1 e2 e3 e4 e5 = group5 UIToolBar e1 e2 e3 e4 e5

toolbarc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
toolbarc ec es = groupc UIToolBar ec es

//# UIButtonBar
buttonbar :: Editor () ()
buttonbar = group UIButtonBar

buttonbarl :: !(Editor a w) -> Editor [a] [(Int,w)]
buttonbarl e = groupl UIButtonBar e

buttonbarL :: ![Editor a w] -> Editor [a] [(Int,w)]
buttonbarL es = groupL UIButtonBar es

buttonbar1 :: !(Editor a w) -> Editor a w
buttonbar1 e1 = group1 UIButtonBar e1

buttonbar2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
buttonbar2 e1 e2 = group2 UIButtonBar e1 e2

buttonbar3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
buttonbar3 e1 e2 e3 = group3 UIButtonBar e1 e2 e3

buttonbar4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
buttonbar4 e1 e2 e3 e4 = group4 UIButtonBar e1 e2 e3 e4

buttonbar5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
buttonbar5 e1 e2 e3 e4 e5 = group5 UIButtonBar e1 e2 e3 e4 e5

buttonbarc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
buttonbarc ec es = groupc UIButtonBar ec es

//# UIList
list :: Editor () ()
list = group UIList

listl :: !(Editor a w) -> Editor [a] [(Int,w)]
listl e = groupl UIList e 

listL :: ![Editor a w] -> Editor [a] [(Int,w)]
listL es = groupL UIList es

list1 :: !(Editor a w) -> Editor a w
list1 e1 = group1 UIList e1

list2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
list2 e1 e2 = group2 UIList e1 e2

list3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
list3 e1 e2 e3 = group3 UIList e1 e2 e3

list4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
list4 e1 e2 e3 e4 = group4 UIList e1 e2 e3 e4

list5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
list5 e1 e2 e3 e4 e5 = group5 UIList e1 e2 e3 e4 e5

listc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
listc ec es = groupc UIList ec es

//# UIListItem
listitem :: Editor () ()
listitem = group UIListItem

listiteml :: !(Editor a w) -> Editor [a] [(Int,w)]
listiteml e = groupl UIListItem e

listitemL :: ![Editor a w] -> Editor [a] [(Int,w)]
listitemL es = groupL UIListItem es

listitem1 :: !(Editor a w) -> Editor a w
listitem1 e1 = group1 UIListItem e1

listitem2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
listitem2 e1 e2 = group2 UIListItem e1 e2

listitem3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
listitem3 e1 e2 e3 = group3 UIListItem e1 e2 e3

listitem4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
listitem4 e1 e2 e3 e4 = group4 UIListItem e1 e2 e3 e4

listitem5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
listitem5 e1 e2 e3 e4 e5 = group5 UIListItem e1 e2 e3 e4 e5

listitemc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
listitemc ec es = groupc UIListItem ec es

//# UIDebug
debug :: Editor () ()
debug = group UIDebug

debugl :: !(Editor a w) -> Editor [a] [(Int,w)]
debugl e = groupl UIDebug e

debugL :: ![Editor a w] -> Editor [a] [(Int,w)]
debugL es = groupL UIDebug es

debug1 :: !(Editor a w) -> Editor a w
debug1 e1 = group1 UIDebug e1

debug2 :: !(Editor a wa) !(Editor b wb) -> Editor (a,b) (Maybe wa,Maybe wb)
debug2 e1 e2 = group2 UIDebug e1 e2

debug3 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) -> Editor (a,b,c) (Maybe wa,Maybe wb,Maybe wc)
debug3 e1 e2 e3 = group3 UIDebug e1 e2 e3

debug4 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) -> Editor (a,b,c,d) (Maybe wa,Maybe wb,Maybe wc,Maybe wd)
debug4 e1 e2 e3 e4 = group4 UIDebug e1 e2 e3 e4

debug5 :: !(Editor a wa) !(Editor b wb) !(Editor c wc) !(Editor d wd) !(Editor e we) -> Editor (a,b,c,d,e) (Maybe wa,Maybe wb,Maybe wc,Maybe wd,Maybe we)
debug5 e1 e2 e3 e4 e5 = group5 UIDebug e1 e2 e3 e4 e5

debugc :: !(Editor Int Int) ![((Maybe a) -> a, Editor a w)] -> Editor (Int,a) (Either Int w)
debugc ec es = groupc UIDebug ec es
