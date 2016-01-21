implementation module Incidone.RoleBased.WatchOfficerTasks

import iTasks, iTasks.API.Extensions.Collection, Data.Tuple, Data.Either, Data.Functor, Text
import qualified Data.Map

import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.IncidentManagementTasks, Incidone.OP.ContactManagementTasks, Incidone.OP.CommunicationManagementTasks
import Incidone.ActionManagementTasks
import Incidone.Util.TaskPatterns

keepWatch :: [Workspace -> Task ()]
keepWatch =	[browseCommunications,browseIncidents,browseContacts,browseActions]

/*
* The communication dashboard shows all ongoing communication.
*/
browseCommunications :: Workspace -> Task ()
browseCommunications ws
    =   selectCommunication
    >^* [OnAction (Action "/Add/Phone/Answer call"   [ActionIcon "phone-call"])                     (always createAndAnswerPhoneCall)
		,OnAction (Action "/Add/Phone/Initiate call" [ActionIcon "phone-call"])                     (always createAndInitiatePhoneCall)
		,OnAction (Action "/Add/Radio/Answer call"   [ActionIcon "vhf-call"])	                    (always createAndAnswerRadioCall)
		,OnAction (Action "/Add/Radio/Initiate call" [ActionIcon "vhf-call"])                       (always createAndInitiateRadioCall)
		,OnAction (Action "/Add/E-mail/Send message" [ActionIcon "email-message"])                  (always createAndComposeEmailMessage)
		,OnAction (Action "/Add/P2000/Send message"  [ActionIcon "p2000-message"])                  (always createAndComposeP2000Message)
		,OnAction (Action "/Open"                    [ActionIcon "open",ActionTrigger DoubleClick]) (hasValue (openCommunication ws))
        ] @! ()
where
	selectCommunication
        = enterChoiceWithSharedAs [Att (Icon "communication"),Att (Title "Communication")] [] allCommunications communicationIdentity
    openCommunication ws communicationNo
        = addToWorkspace ((doOrClose (updateCommunication communicationNo)) <<@ InWindow) ws @! ()

    createAndAnswerPhoneCall     = (doOrClose (createCommunication PhoneCall In  Nothing >>- answerPhoneCall   )) <<@ InWindow @!()
    createAndInitiatePhoneCall   = (doOrClose (createCommunication PhoneCall Out Nothing >>- initiatePhoneCall )) <<@ InWindow @!()
    createAndAnswerRadioCall     = (doOrClose (createCommunication RadioCall In  Nothing >>- answerRadioCall   )) <<@ InWindow @!()
    createAndInitiateRadioCall   = (doOrClose (createCommunication RadioCall Out Nothing >>- initiateRadioCall )) <<@ InWindow @!()
    createAndComposeEmailMessage = (doOrClose (createCommunication EmailMessage Out Nothing >>- composeEmailMessage)) <<@ InWindow @!()
    createAndComposeP2000Message = (doOrClose (createCommunication P2000Message Out Nothing >>- composeP2000Message)) <<@ InWindow @!()

/*
* The incident dashboard gives an overview of all open incidents,
* provides ad-hoc creation of new incidents and provides 
* a way to open the incident information browsing incident task for open incidents.
*/
browseIncidents :: Workspace -> Task ()
browseIncidents ws
	=	feedForward
    (   selectIncident
        >^* [OnAction (Action "/Add incident" [ActionIcon "add"]) (always (createNewIncident <<@ InWindow @! ()))
            ,OnAction (Action "/Open" [ActionIcon "open",ActionTrigger DoubleClick]) (hasValue (\i -> openIncidentInWorkspace ws i @! ()))
            ]
    )
	(	withSelection viewNoSelection viewIncidentDetails
	)	<<@ LazyRefresh <<@ (ArrangeWithSideBar 1 RightSide 300 True) <<@ (Icon "incidents") <<@ (Title "Incidents")
    @! ()
where
	selectIncident
		= ( (enterChoiceWithSharedAs (Title "Open incidents")
			    [] openIncidentsDetails (\{IncidentDetails|incidentNo} -> incidentNo) /* <<@ AfterLayout (tweakUI fill) */) //FIXME
            -||-
            (enterChoiceWithSharedAs (Title "Recent incidents")
                [] recentIncidentsDetails (\{IncidentDetails|incidentNo} -> incidentNo) /* <<@ AfterLayout (tweakUI fill) */) //FIXME
         ) <<@ ArrangeWithTabs

browseContacts :: Workspace -> Task ()
browseContacts ws
	=	feedForward
    (   selectContact
        >^* [(OnAction (Action "/Add contact" [ActionIcon "add"]) (always (addContact <<@ InWindow @! ())))
            ,(OnAction (Action "/Open" [ActionIcon "open",ActionTrigger DoubleClick]) (ifValue (\c-> c=:(Left _)) (\(Left c) -> (openContactInWorkspace ws c) @! ())))
            ,(OnAction (Action "/Quick update/Position" [ActionIcon "map"]) (ifValue (\c-> c=:(Left _)) (\(Left c) -> updateContactPosition c <<@ InWindow @! ())))
            ,(OnAction (Action "/Quick update/Status" [ActionIcon "edit"]) (ifValue (\c-> c=:(Left _)) (\(Left c) -> updateContactStatus c <<@ InWindow @! ())))
            ]
    )
	(   withSelection viewNoSelection viewDetails
	)	<<@ LazyRefresh <<@ (ArrangeWithSideBar 1 RightSide 300 True) <<@ (Icon "contacts") <<@ (Title "Contacts")
    @! ()
where
    viewDetails (Left contactNo)    = viewContactDetails contactNo
    viewDetails (Right mmsi)        = viewAISContactDetails mmsi

    addContact :: Task (Maybe ContactNo)
    addContact
        =   enterInformation (Title "Add contact") []
        >>? createContact

:: ActionSet = PersonalActions | IncidentActions !IncidentNo

:: ActionShort =
	{	status		:: Maybe String
	,	title		:: Maybe String
	,	createdOn	:: Maybe String
	,	createdBy	:: Maybe String	
	}

derive class iTask ActionSet, ActionShort

browseActions :: Workspace -> Task ()
browseActions ws
	=	selectAndWorkOnActions
    >^* [OnAction (Action "/Add action" [ActionIcon "add"]) (always (addTopActionItem [] []))]
	@!  ()
where
	selectAndWorkOnActions
     = feedForward (chooseActionItem (Title "Overview") True True actionStatuses /* <<@ AfterLayout (tweakUI fill) */) //FIXME
        (\s -> whileUnchanged s
            (\t -> case t of
              Just taskId    = workOnActionItem taskId
              Nothing        = viewInformation () [] ()
            )
        ) <<@ (ArrangeWithSideBar 0 LeftSide 250 True) <<@ (Icon "actions") <<@ (Title "Actions")



