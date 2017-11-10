implementation module iTasks.API.Extensions.Distributed.InteractionTasks

from iTasks.WF.Definition import class iTask, :: TaskValue
from iTasks.Internal.SDS import :: SDS, :: ReadWriteShared, :: RWShared
from iTasks.WF.Definition      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from iTasks.WF.Tasks.Interaction		import :: ViewOption(..), viewInformation
from iTasks.UI.Prompt import class toPrompt, instance toPrompt String 

from iTasks.WF.Tasks.SDS import watch
from iTasks.WF.Combinators.Common import >>-, >>*, ||-, hasValue, ifValue, :: TaskCont(OnValue)
from iTasks.WF.Combinators.Overloaded import class TApplicative(return), instance Functor Task, instance TApplicative Task
from Data.Functor import class Functor
from GenEq import =!=

viewSharedInformation :: String [ViewOption r] !(ReadWriteShared r w) -> Task r | iTask r & iTask w
viewSharedInformation title options share
        = watch share
        >>* [OnValue (hasValue return)]
        >>- \v -> loop v title options share
where
        loop :: r String [ViewOption r] (ReadWriteShared r w) -> Task r | iTask r & iTask w
        loop v title options share
                = (viewInformation title options v)
                ||- (watch share >>* [OnValue (ifValue ((=!=) v) return)])
                >>- \v -> loop v title options share
