definition module iTasks.API.Extensions.Distributed.Task

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)

:: Requires = Requires String

class (@:) infix 3 u a :: u a -> a

instance @: worker (Task a) | iTask a & toUserConstraint worker

instance @: Domain (Task a) | iTask a

instance @: DomainUser (Task a) | iTask a

instance @: Requires (Task a) | iTask a

:: Domain = Domain String
:: DomainUser = E. a: DomainUser a Domain & toUserConstraint a & gText{|*|} a & toString a

derive class iTask Domain
derive gText DomainUser	

instance toString DomainUser

class (@.) infix 4 u a :: u a -> DomainUser

instance @. worker Domain | toUserConstraint worker & gText{|*|} worker & toString worker
