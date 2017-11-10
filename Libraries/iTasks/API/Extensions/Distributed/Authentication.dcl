definition module iTasks.API.Extensions.Distributed.Authentication

from iTasks._Framework.Generic import class iTask
from iTasks.API.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from iTasks.API.Extensions.User import class toUserConstraint(..), :: UserConstraint
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization    import :: TextFormat(..)
from iTasks.API.Extensions.Distributed.Task import :: Domain
from iTasks.API.Extensions.User import :: User, :: Username, ::Password
from iTasks._Framework.SDS import :: RWShared, :: ROShared

remoteAuthenticateUser	:: !Username !Password	-> Task (Maybe User)

domainAuthServer :: Task ()

/*
 * Get users from domain.
 */
usersOf :: Domain -> Task [User]

startAuthEngine :: Domain -> Task ()

enterDomain :: Task Domain

currentDistributedUser :: RWShared () (User,Domain) (User,Domain)

currentDomain :: ROShared () Domain
