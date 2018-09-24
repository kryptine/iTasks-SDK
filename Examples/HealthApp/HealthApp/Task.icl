implementation module HealthApp.Task

import iTasks
import HealthApp.Definition
import HealthApp.SDS

viewMedication :: Task ()
viewMedication = selectClient
	>>= \clientId. viewSharedInformation "Medication" [] (clientMedicationShare clientId)
	@! ()

updateAddress :: Task ()
updateAddress = selectClient
	>>= \clientId. updateSharedInformation "Update address" [] (clientAddresShare clientId)
	@! ()

createClient :: Task ()
createClient = enterInformation "Please enter client details" []
	>>= \client. upd (\l. [client : l]) clientsShare
	@! ()

registerMedication :: Task ()
registerMedication = selectClient
	>>= \clientId. enterInformation "Enter new medication" []
	>>= \medication. upd (\ms. [medication : ms]) (clientMedicationShare clientId)
	@! ()

createAppointment :: Task ()
createAppointment = treturn ()

selectClient :: Task ClientId
selectClient = enterChoiceWithSharedAs "Select client" [ChooseFromList \(_, client). client.Client.name] clientsIndexedShare (\(clientId, _). clientId)