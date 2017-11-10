implementation module iTasks.API.Extensions.Distributed.Task

import StdString
import StdInt

from iTasks._Framework.Generic import class iTask
from iTasks.API.Core.Types import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskAttributes, :: DateTime, instance toString DateTime
from Data.Maybe import :: Maybe, maybe
from iTasks.API.Extensions.User import class toUserConstraint(..), :: UserConstraint, instance toString UserConstraint, instance toUserConstraint User, instance toString UserConstraint
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization import :: TextFormat(..)
import qualified iTasks.API.Extensions.User as U
import Data.Map
from symbols_in_program import :: Symbol
from iTasks.API.Extensions.Distributed.RemoteTask import remoteAssignTask
from iTasks.API.Common.TaskCombinators import -&&-, >>-
from iTasks.API.Core.SDSs import currentDateTime
from iTasks.API.Extensions.User import currentUser, :: User(..), :: UserTitle, :: Role, :: UserId, :: SessionId, assign, workerAttributes
from iTasks._Framework.SDS import :: ReadWriteShared, :: RWShared, :: ReadOnlyShared, :: ROShared
from iTasks.API.Extensions.Distributed.Authentication import currentDomain
import qualified iTasks.API.Core.Tasks as C
import iTasks.API.Extensions.Distributed.Engine

instance @: worker (Task a) | iTask a & toUserConstraint worker
where
	(@:) worker task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> assign (workerAttributes worker
			[ ("title",      toTitle worker)
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			, ("createdFor", toString (toUserConstraint worker))
			]) task

instance @: Domain (Task a) | iTask a
where
	(@:) domain task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> remoteAssignTask (fromList
			[ ("title",      "None")
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			]) task domain

instance @: DomainUser (Task a) | iTask a
where
	(@:) (DomainUser worker domain) task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> remoteAssignTask (fromList
			[ ("title",      toTitle worker)
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			, ("createdFor", toString (toUserConstraint worker))
			]) task domain

instance @: Requires (Task a) | iTask a
where
	(@:) (Requires requires) task
		= 'C'.get currentUser -&&- 'C'.get currentDateTime
		>>- \(me,now) -> 'C'.get currentDomain
		>>- \domain -> remoteAssignTask (fromList
			[ ("title",      "None")
			, ("createdBy",  toString (toUserConstraint me))
			, ("createdAt",  toString now)
			, ("priority",   toString 5)
			, ("requires",   requires)
			]) task domain

derive class iTask Domain

instance @. worker Domain | toUserConstraint worker & gText{|*|} worker & toString worker
where
	(@.) worker domain 
		= (DomainUser worker domain)

gText{|DomainUser|} format user = maybe [""] (\(DomainUser worker _) -> gText{|*|} format (Just worker)) user

instance toString DomainUser
where
	toString (DomainUser worker _) = toString worker

