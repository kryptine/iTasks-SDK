definition module iTasks.Extensions.Distributed.Authentication

import iTasks

import iTasks.Extensions.Distributed.Task
//from iTasks._Framework.Generic import class iTask
//from iTasks.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
//from Data.Maybe import :: Maybe
//from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint
//from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
//from iTasks._Framework.Generic.Visualization    import :: TextFormat(..)
//from iTasks.Extensions.Distributed.Task import :: Domain
//from iTasks.Extensions.User import :: User, :: Username, ::Password
//from iTasks._Framework.SDS import :: RWShared, :: ROShared

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
