module OnClicks

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.WF.Combinators.SDS
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdFunctions, StdArray, StdInt, StdList, StdReal, StdTuple
from   iTasks import instance Identifiable SDSLens, instance Modifiable SDSLens, instance Registrable SDSLens, instance Readable SDSLens, instance Writeable SDSLens

//	shorthand definitions for the used fonts in these examples
times			= normalFontDef "Times New Roman"

//	shorthand definitions for the used colours in these examples
black			= toSVGColor "black"
white			= toSVGColor "white"
yellow          = toSVGColor "yellow"
none            = toSVGColor "none"

Start :: *World -> *World
Start world
	= startEngine [publish "/" (const on_clicks)] world

:: Toggles = {value_in_sds :: Int, update_a_locally :: Bool, update_b_locally :: Bool}
derive class iTask Toggles
derive JSEncode Toggles
derive JSDecode Toggles

:: Who = A | B
derive class iTask Who
derive JSEncode Who

toggleOf :: Who Toggles -> Bool
toggleOf A t = t.Toggles.update_a_locally
toggleOf B t = t.Toggles.update_b_locally

toggleIncr :: Int Toggles -> Toggles
toggleIncr n t=:{Toggles | value_in_sds = m} = {Toggles | t & value_in_sds = n+m}

toggle :: Who Toggles -> Toggles
toggle A t=:{Toggles | update_a_locally} = {Toggles | t & update_a_locally = not update_a_locally}
toggle B t=:{Toggles | update_b_locally} = {Toggles | t & update_b_locally = not update_b_locally}

/** on_clicks:
	creates three parallel tasks that are connected to the same SDS of type Toggles. 
	The first two parallel tasks allow the user to alter the shared value by means of mouse clicks.
	The third parallel task allows the user to view and update the same value.
	Toggles contains an Int value (initially 0) and two Bool values that cause edits to the Int value to 
	be local (True) or shared (False) in the first two parallel tasks.
*/
on_clicks :: Task Toggles
on_clicks
	= withShared {Toggles | value_in_sds = 0, update_a_locally = False, update_b_locally = False} 
	             (\sds = on_click A sds -||- on_click B sds -||- edit_value sds)

/** on_click who sds = task:
	@task is connected with @sds to display its current Int value, and allows the user to increase that value
	by clicking on the rendering. If the corresponding Bool value of @who is True, then these edits are local,
	and if it is False, then these edits are shared.
*/
on_click :: Who (sds () Toggles Toggles) -> Task Toggles | RWShared sds
on_click label sds
	= updateSharedInformation ("On Click " <+++ label)
	                          [UpdateUsing id (\_ v = v) (fromSVGEditor
                                                          { initView    = id
                                                          , renderImage = const (count label)
                                                          , updModel    = \_ v = v
                                                          })] sds

/**	count who toggles tags = image:
	@image displays the number in @toggles and allows the user to alter the value by means of mouse clicks.
	These edits are propagated to the shared value only if @who in @toggles indicates False.
*/
count :: Who Toggles *TagSource -> Image Toggles
count label toggles _
	= margin (px 20.0) (
		beside (repeat AtMiddleY) [] Nothing [] (
		  [ beside [] [] Nothing [] (map digit (digits n)) NoHost <@< {onclick = toggleIncr, local = toggleOf label toggles}
		  , margin (px 10.0) (
		       circle (h /. 5) 
		           <@< {onclick     = const (toggle label), local = False}
		           <@< {stroke      = if (toggleOf label toggles) black none}
		           <@< {strokewidth = if (toggleOf label toggles) (h /. 25) (h /. 50)}
		           <@< {fill        = yellow}
		    )
		  , margin (px 10.0) (text small_font (if (toggleOf label toggles) "local edits ON" "local edits OFF"))
		  ]) NoHost
	  )
where
	big_font   = times h
	small_font = times (h / 10.0)
	h          = 100.0
	m          = 6.0
	n          = toggles.Toggles.value_in_sds
	
	digits :: Int -> [Int]
	digits n = [toInt c - toInt '0' \\ c <-: toString n]
	
	digit :: Int -> Image m
	digit n = overlay [(AtMiddleX,AtMiddleY)] []
	             [ text big_font (toString n) <@< {fill = white}]
	             (Host (rect (textxspan big_font (toString n) + px m) (px (h+m))))

/** edit_value sds = task:
	@task allows the user to view and alter the Int value of the Toggles SDS.
*/
edit_value :: (sds () Toggles Toggles) -> Task Toggles | RWShared sds
edit_value sds
	= updateSharedInformation "Current value in SDS" [UpdateAs (\t = t.Toggles.value_in_sds) (\t v = {Toggles | t & value_in_sds=v})] sds
