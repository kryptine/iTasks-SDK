implementation module Incidone.OP.ImportExportTasks
import iTasks
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.ContactManagementTasks

importContactsFromCSVFile :: Document -> Task ()
importContactsFromCSVFile doc
    =   importCSVDocument doc
    >>- \csv -> sequence "Importing contacts" (map create (skipHeader csv))
    @!  ()
where
    skipHeader [] = []
    skipHeader [_:rows] = rows

    create [name,group,type,subtype,providesHelp,mmsi,p2000,phone,notes:_]
        =   createContact {NewContact|type = Just (ctype type),name = Just name, position = Nothing, needsHelp = False /*, communicationMeans = cmeans phone mmsi p2000*/}
		>>- \contactNo ->
            upd (\c -> {Contact|c & providesHelp = providesHelp == "y", group = Just group, notes = cnotes notes}) (sdsFocus contactNo contactByNo)
        @!  ()
    create _ = return ()

	ctype "Person" 	    = Person
	ctype "Vessel" 	    = Vessel
	ctype "Airplane"	= Airplane
	ctype "Helicopter"	= Helicopter
	ctype _				= OtherContact

    cmeans phoneNo mmsi capCode
        =   if (phoneNo == "") [] [{NewCommunicationMean|defaultValue&type=CMPhone,phoneNo=Just phoneNo}]
        ++  if (mmsi == "") [] [{NewCommunicationMean|defaultValue&type=CMVHF,mmsi=Just (toInt mmsi)}]
        ++  if (capCode == "") [] [{NewCommunicationMean|defaultValue&type=CMP2000,capCode=Just capCode}]

    cnotes "" = Nothing
    cnotes note = Just (Note note)
