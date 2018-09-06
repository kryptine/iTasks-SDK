definition module Incidone.OP.ContactManagementTasks
import iTasks
import Incidone.OP.Concepts, Incidone.Util.Workspace

//Browsing contacts
selectContact                   :: Task (Either ContactNo MMSI)

//Core contact information management tasks
openContactInWorkspace          :: Workspace ContactNo -> Task ()
manageContactInformation        :: Workspace ContactNo -> Task ()

manageContactBasics             :: ContactNo    -> Task ()
manageContactDetails            :: ContactNo    -> Task ()
manageContactPhotos             :: ContactNo    -> Task ()
manageContactAccess             :: ContactNo    -> Task ()
manageContactActions            :: Bool ContactNo    -> Task ()
manageContactCommunicationMeans :: Bool ContactNo    -> Task CommunicationMean

//Summary tasks
viewContactDetails              :: ContactNo    -> Task ()
viewAISContactDetails           :: MMSI         -> Task ()

viewContactHeader               :: ContactNo    -> Task ()
viewContactCommunicationMeans   :: ContactNo    -> Task [CommunicationMean]

//Reusable task fragments
updateContactPosition           :: ContactNo -> Task (Maybe (Maybe ContactPosition))
updateContactStatus             :: ContactNo -> Task (Maybe (Maybe ContactStatus))

updateSharedContactRefList      :: d (sds () [ContactNo] [ContactNo]) -> Task [ContactNo] | toPrompt d & RWShared sds
selectKnownOrDefineNewContact   :: Task (Either ContactNo NewContact)
createContactIfNew              :: (Either ContactNo NewContact) -> Task ContactNo

createContact			        :: NewContact -> Task ContactNo

deleteContact			        :: ContactNo -> Task ()

addContactPhoto                 :: ContactNo Document -> Task ContactPhoto
updatePosition			        :: ContactPosition String (sds () Contact Contact) -> Task Contact | RWShared sds

createCommunicationMean         :: ContactNo NewCommunicationMean -> Task CommunicationMeanId
deleteCommunicationMean         :: CommunicationMeanId -> Task ()

//Check credentials for contacts that can log in
verifyContactCredentials        :: Credentials -> Task (Maybe User)

viewContactsOnMap           :: (sds1 () [ContactGeo] w) (sds2 () (Maybe (Either ContactNo MMSI)) (Maybe (Either ContactNo MMSI))) -> Task (Either ContactNo MMSI) | iTask w & RWShared sds1 & RWShared sds2
