definition module iTasks.Extensions.Distributed.iTasks

import StdBool
import StdString
import StdInt
import StdTuple
import StdFile
import StdOrdList
from StdFunc import const, o

import System.OS

from iTasks.WF.Definition import class iTask
from iTasks.Internal.Task import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskAttributes
from Data.Maybe import :: Maybe
from iTasks.Extensions.User import class toUserConstraint(..), :: UserConstraint, instance toString UserConstraint, instance toUserConstraint User, instance toString UserConstraint, instance toString User
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization import :: TextFormat(..)
import qualified iTasks.Extensions.User as U
from iTasks.WF.Combinators.Common import -&&-, >>-
from iTasks.SDS.Sources.System import currentDateTime
from iTasks.Extensions.User import currentUser, :: User(..), :: UserTitle, :: Role, :: UserId, assign, workerAttributes, :: Password, :: Username, workAs, :: Credentials{..}, users
from iTasks.SDS.Definition import :: ReadWriteShared, :: RWShared, :: ReadOnlyShared
from iTasks.WF.Tasks.Core import accWorld
import iTasks.Internal.Distributed.Symbols
from iTasks.Extensions.Distributed.Instance import instanceServer, instanceClient, instanceFilter, instanceClameFilter
from Data.Map import :: Map
from iTasks.Extensions.Admin.WorkflowAdmin import workflow, class toWorkflow(..), :: Workflow, publish, :: PublishedTask{..}, :: TaskWrapper(..), manageWorklist, instance toWorkflow (Task a), instance toWorkflow (WorkflowContainer a), instance toWorkflow (a -> Task b), instance toWorkflow (ParamWorkflowContainer a b), :: WorkflowContainer, :: ParamWorkflowContainer
from System.FilePath import :: FilePath, </>
from iTasks.Engine import startEngineWithOptions, :: EngineOptions(..), startEngine, class Publishable, instance Publishable [PublishedTask], :: WebTaskWrapper
from Internet.HTTP import :: HTTPRequest(..), :: HTTPUpload, :: HTTPProtocol, :: HTTPMethod
import iTasks.WF.Combinators.Common
from iTasks.WF.Combinators.Common import :: TaskCont
from iTasks.WF.Tasks.Interaction import enterInformation, :: EnterOption, :: ViewOption, enterChoice, :: ChoiceOption, viewInformation, enterChoiceWithShared, updateInformationWithShared, updateSharedInformation, :: UpdateOption
from iTasks.Extensions.DateTime import :: DateTime, :: Time, waitForTimer
from iTasks.Extensions.Admin.UserAdmin import manageUsers, loginAndManageWorkList
from iTasks.SDS.Sources.System import currentTime
from iTasks.SDS.Sources.Store import sharedStore
from iTasks.WF.Combinators.SDS import withShared
from iTasks.Internal.Distributed.Domain import :: Domain
import iTasks.Extensions.Distributed.Task
import iTasks.Extensions.Distributed.SDS
from iTasks.Extensions.Distributed.Authentication import domainAuthServer, usersOf, remoteAuthenticateUser, startAuthEngine, enterDomain, currentDistributedUser, currentDomain
import Text
import Graphics.Scalable
import iTasks.Extensions.Distributed.InteractionTasks
from StdList import ++
import iTasks.WF.Combinators.Overloaded

from Internet.HTTP import :: HTTPResponse{..}, :: HTTPMethod(..)
from Text.URI import :: URI{..}, parseURI
from iTasks.Extensions.Web import callHTTP

from iTasks.UI.Prompt import :: Title(..), instance toPrompt Title, class toPrompt, instance toPrompt String, instance toPrompt ()

from iTasks.Extensions.Device.Features import hasCamera, device, :: DeviceFeatures, manageDeviceFeaturs
from iTasks.Extensions.Picture.JPEG import :: JPEGPicture(..)
from iTasks.Extensions.Device.Camera import takePicture
from iTasks.Extensions.Device.Location import :: Coordinates(..), getLocation

from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode, jsonQuery, :: JSONNode(JSONNull), instance toString JSONNode, instance fromString JSONNode

from StdList import isEmpty
from StdOverloaded import class toReal
import StdReal
from Data.Error import :: MaybeError(..)
