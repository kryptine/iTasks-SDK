definition module iTasks.Extensions.Distributed.Authentication

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from iTasks.Extensions.Distributed.Task import :: Domain
from iTasks.Extensions.User import :: User, :: Username, ::Password
from iTasks.SDS.Definition import :: SDS, :: RWShared, :: ROShared

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
