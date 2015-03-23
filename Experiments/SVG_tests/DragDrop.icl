module DragDrop

import StdDebug
import iTasks
import iTasks.API.Extensions.SVG.SVGlet
import qualified Data.Set as DS
import Graphics.Scalable.Internal

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> dragDrop)
                          ] world

:: DragDropState =
  { xcoord :: Real
  , ycoord :: Real
  , mousedown :: Bool
  , targetColor :: String
  }

derive class iTask DragDropState

defaultState :: DragDropState
defaultState = { DragDropState | xcoord = 0.0, ycoord = 0.0, mousedown = False, targetColor = "green" }

dragDropState :: Shared DragDropState
dragDropState = sharedStore "dragDropState" defaultState

dragDrop :: Task DragDropState
dragDrop = updateSharedInformation "Drag and drop example" [imageUpdate id mkImg (\_ _ -> Nothing) (const id)] dragDropState

mkImg :: DragDropState *TagSource -> Image DragDropState
mkImg ddst tsrc
  #! (target, tref, tsrc) = tagWithSrc tsrc (rect (px 100.0) (px 100.0) <@< { fill = toSVGColor ddst.targetColor })
  #! (targetTag, tref)    = tagFromRef tref
  #! tsrc = trace_n (toString targetTag) tsrc
  #! box    = rect (px 50.0) (px 50.0) <@< { draggable = Just (\targetTags x y ddst -> {ddst & xcoord = x, ycoord = y, targetColor = changeColor targetTag targetTags })}
  #! canvas = rect (px 500.0) (px 500.0) <@< { fill = toSVGColor "none" }
  = collage [(px 200.0, px 200.0), (px ddst.xcoord, px ddst.ycoord)] [target, box] (Just canvas)
  where
  changeColor targetTag (Just targetTags)
    | 'DS'.member targetTag targetTags = "red"
  changeColor _ _ = "white"
