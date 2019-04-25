module WasmTest

import StdEnv
import Data.Error
from Data.Func import $

import iTasks.Engine
import iTasks.Internal.Client.Serialization
import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.JS.Interface
import iTasks.UI.Prompt
import iTasks.WF.Tasks.Interaction

Start w = doTasks task w

import iTasks.Extensions.DateTime
import iTasks.Internal.SDS
import iTasks.SDS.Sources.System
import iTasks.WF.Combinators.Common
import iTasks.WF.Combinators.SDS

task :: Task Date
task = withShared {Date|year=2019,mon=4,day=9} \sds ->
	viewSharedInformation "view" [] sds -||-
	updateSharedInformation "update 1" [] sds -||-
	updateSharedInformation "update 2" [] sds

task = updateInformation "test"
	[ UpdateUsing (\m -> m) (\_ v -> v) $ leafEditorToEditor
		{ LeafEditor
		| genUI          = withClientSideInit initUI genUI
		, onEdit         = \_ (_,st) _ vst -> (Ok (NoChange, st), vst)
		, onRefresh      = \_ new old vst
			| new == old -> (Ok (NoChange, new), vst)
			| otherwise  -> undef // TODO: serialize
		, valueFromState = Just
		}
	]
	37
where
	initUI :: !(JSObj ()) !*JSWorld -> *JSWorld
	initUI comp w
	# w = (jsGlobal "console.log" .$! (1,2,3)) w
	# w = (comp .# "reversed_list" .= jsMakeCleanReference (MyReverse 1000)) w
	# (v,w) = (jsGlobal "Math.floor" .$ 17) w
	# (jsInitDOMEl,w) = jsWrapFun (initDOMEl comp) w
	# w = (comp .# "initDOMEl" .= jsInitDOMEl) w
	# (jsAfterInitDOM,w) = jsWrapFun afterInitDOM w
	# w = (comp .# "afterInitDOM" .= jsAfterInitDOM) w
	= w
	where
		initDOMEl :: !(JSObj ()) !{!JSVal a} !*JSWorld -> *JSWorld
		initDOMEl comp _ w
		# (v,w) = jsGetCleanReference (comp .# "reversed_list") w
		# v = case v of
			Nothing -> "stored value not found?"
			Just xs -> toString (last` xs)
				with
					last` :: ![Int] -> Int
					last` xs = last xs
		# w = (comp .# "domEl.value" .= toJS v) w
		# w = (comp .# "afterInitDOM" .$! ()) w
		= w

		afterInitDOM :: !{!JSVal a} !*JSWorld -> *JSWorld
		afterInitDOM _ w
		# w = (jsGlobal "console.log" .$! "element initialized") w
		= w

	genUI :: !UIAttributes !DataPath !(EditMode s) !*VSt -> *(!MaybeErrorString (!UI, !s), !*VSt)
	genUI attr dp mode vst = case editModeValue mode of
		Nothing
			-> (Error "cannot be in enter mode", vst)
		Just val
			# (s,vst) = serialize_in_vst val vst
			-> (Ok (ui UITextField, val), vst)

MyReverse::Int -> [Int]
MyReverse n =  Rev_n n [1..n]
where
	Rev_n::Int [Int] -> [Int]
	Rev_n 1 list	=  Rev list []
	Rev_n n list	=  Rev_n (n - 1) (Rev list [])

	Rev::[Int] [Int] -> [Int]
	Rev [x:r]	list	=  Rev r [x : list]
	Rev []		list	=  list