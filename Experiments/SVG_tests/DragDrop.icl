module DragDrop

import StdDebug
import iTasks
import iTasks.API.Extensions.SVG.SVGlet

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> dragDrop)
                          ] world

:: DragDropState =
  { xcoord :: Real
  , ycoord :: Real
  , mousedown :: Bool
  }

derive class iTask DragDropState

defaultState :: DragDropState
defaultState = { DragDropState | xcoord = 0.0, ycoord = 0.0, mousedown = False }

dragDropState :: Shared DragDropState
dragDropState = sharedStore "dragDropState" defaultState

dragDrop :: Task DragDropState
dragDrop = updateSharedInformation "Drag and drop example" [imageUpdate id mkImg (\_ _ -> Nothing) (const id)] dragDropState

mkImg :: DragDropState *TagSource -> Image DragDropState
mkImg ddst tsrc = collage [(px ddst.xcoord, px ddst.ycoord)] [box] (Just canvas)
  where
  canvas = rect (px 500.0) (px 500.0) <@< { fill = toSVGColor "none" }
  box = rect (px 50.0) (px 50.0) <@< { draggable = Just (\x y ddst -> {ddst & xcoord = x, ycoord = y})}
