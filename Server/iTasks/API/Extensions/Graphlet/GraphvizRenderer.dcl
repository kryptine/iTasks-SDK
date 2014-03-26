definition module iTasks.API.Extensions.Graphlet.GraphvizRenderer

from Data.Maybe import :: Maybe
import iTasks
from iTasks.API.Extensions.Graphlet.Graphlet import :: GraphletRenderer

:: GraphvizShape
  = GSBox (Maybe [String])
  | GSEllipse (Maybe [String])
  | GSCircle (Maybe [String])
  | GSPoint
  | GSTriangle (Maybe [String])
  | GSPlainText [String]
  | GSDiamond (Maybe [String])
  | GSSquare (Maybe [String])
  | GSNone

derive class iTask GraphvizShape

:: GraphvizEdge :== Maybe [String]

graphvizRenderer :: GraphletRenderer Void GraphvizShape GraphvizEdge
