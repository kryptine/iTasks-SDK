definition module iTasks.API.Extension.Tonic.Toniclet

from iTasks.Framework.Tonic.AbsSyn import :: GinGraph, :: Graph, :: GEdge, :: GNode
from iTasks.API.Core.Client.Editlet import :: Editlet

toniclet :: GinGraph -> Editlet GinGraph GinGraph
