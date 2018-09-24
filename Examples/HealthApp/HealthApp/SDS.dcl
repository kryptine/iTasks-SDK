definition module HealthApp.SDS

import HealthApp.Definition

clientsShare :: SDSLens () [Client] [Client]

clientsIndexedShare :: SDSLens () [(ClientId, Client)] [(ClientId, Client)]

clientAddresShare :: ClientId -> SDSLens () Address Address

clientMedicationShare :: ClientId -> SDSLens () [Medication] [Medication]