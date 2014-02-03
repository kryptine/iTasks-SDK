definition module iTasks.API.Extensions.Tonic.TonicRenderer

from Data.Maybe import :: Maybe
import iTasks
from iTasks.Framework.Tonic import :: TonicTrace
from iTasks.Framework.Tonic.AbsSyn import :: GNode, :: GEdge
from iTasks.API.Extensions.Tonic.Toniclet import :: GraphletRenderer

:: TonicState = TonicState [TonicTrace]
:: TonicDiff = TonicDiff

derive class iTask TonicState, TonicDiff

tonicRenderer :: GraphletRenderer GNode GEdge
