definition module Incidone.OP.IncidentManagementTasks
import iTasks
import Incidone.OP.Concepts, Incidone.ActionManagementTasks, Incidone.Util.Workspace

//Core contact information management tasks
openIncidentInWorkspace     :: Workspace IncidentNo -> Task ()
manageIncidentInformation   :: Workspace IncidentNo -> Task ()

manageIncidentLog           :: IncidentNo -> Task ()

//Summary tasks
viewIncidentDetails         :: IncidentNo -> Task ()

//Reusable task fragments
updateSharedIncidentRefList     :: d Bool (RWShared () [IncidentNo] [IncidentNo]) -> Task [IncidentNo] | toPrompt d

selectKnownOrDefineNewIncident  :: Task (Either IncidentNo NewIncident)
createIncidentIfNew             :: (Either IncidentNo NewIncident) -> Task IncidentNo

addLogMessage 			    :: msg IncidentNo -> Task IncidentNo | toString msg

logCommunicationResponded   :: CommunicationNo -> Task ()                               // Add to all incidents mentioned in communication
logIncidentCreated          :: IncidentNo NewIncident -> Task ()                        // Initial situation
logIncidentBasicsUpdated    :: IncidentNo IncidentBasic IncidentBasic -> Task ()        // Change in incident info
logIncidentWeatherUpdated   :: IncidentNo WeatherData WeatherData -> Task ()            // Updated weather info
logContactAdded             :: IncidentNo ContactNo -> Task ()                          // Contact involved in incident
logContactRemoved           :: IncidentNo ContactNo -> Task ()                          // Contact no longer involved in incident
logContactBasicsUpdated     :: ContactNo ContactBasic ContactBasic -> Task ()           // Updated basic information (add to all incidents in which involved)
logContactPhotoAdded        :: ContactNo ContactPhoto -> Task ()                        // Contact photo added (add to all incidents in which involved)
logContactPositionUpdated   :: ContactNo (Maybe ContactPosition) (Maybe ContactPosition) -> Task ()// Updated position (add to all incidents in which involved)
logContactStatusUpdated     :: ContactNo (Maybe ContactStatus) (Maybe ContactStatus) -> Task ()// Updated status (add to all incidents in which involved)
logPersonDetailsUpdated     :: ContactNo PersonDetails PersonDetails -> Task ()
logVesselDetailsUpdated     :: ContactNo VesselDetails VesselDetails -> Task ()
logSurferDetailsUpdated     :: ContactNo SurferDetails SurferDetails -> Task ()
logDiverDetailsUpdated      :: ContactNo DiverDetails DiverDetails -> Task ()
logAirplaneDetailsUpdated   :: ContactNo AirplaneDetails AirplaneDetails -> Task ()
logHelicopterDetailsUpdated :: ContactNo HelicopterDetails HelicopterDetails -> Task ()
logActionAdded              :: ActionStatus -> Task ()
logActionUpdated            :: ActionStatus -> Task ()

createIncident			    :: NewIncident	-> Task IncidentNo
deleteIncident			    :: IncidentNo -> Task ()

closeIncident			    :: IncidentNo -> Task ()
linkContactsToIncident      :: [ContactNo] IncidentNo -> Task IncidentNo
