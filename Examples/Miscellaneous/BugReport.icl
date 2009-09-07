implementation module BugReport

import iTasks
import CommonDomain


:: Application =
	{ appNr			:: AppNr
	, name			:: String
	, versions		:: [String]
	, developers	:: [UserId]
	}
	
:: BugReport =
	{ application	:: DBRef Application
	, version		:: Maybe String
	, severity		:: BugSeverity
	, description	:: Note
	}
	
:: Bug =
	{ bugNr			:: BugNr
	, reportedAt	:: (Date,Time)
	, reportedBy	:: UserId
	, report		:: BugReport
	, status		:: BugStatus
	}

:: AppNr :== Int
:: BugNr :== Int
:: BugSeverity = Low | Medium | High | Critical
:: BugStatus = Reported | Assigned | Reproduced | Resolved

derive gPrint     Application, BugReport, Bug, BugSeverity, BugStatus	
derive gParse	  Application, BugReport, Bug, BugSeverity, BugStatus
derive gVisualize Application, BugReport, Bug, BugSeverity, BugStatus
derive gUpdate	  Application, BugReport, Bug, BugSeverity, BugStatus

instance DB Application where
	databaseId					= mkDBid "Application"
	getItemId {appNr}			= DBRef appNr
	setItemId (DBRef appNr) app	= {app & appNr = appNr}
	
instance DB Bug where
	databaseId					= mkDBid "Bug"
	getItemId bug=:{bugNr}		= DBRef bugNr
	setItemId (DBRef bugNr) bug	= {bug & bugNr = bugNr}

bugReportExample :: [Workflow]
bugReportExample
	= [workflow "Examples/Miscellaneous/Bug report" reportBug]

reportBug :: Task Void
reportBug
	=	enterInitialReport
	>>= \report ->
		fileBugReport report
	>>= \bugnr ->
		case report.severity of
			Critical 
				= selectDeveloper report.application report.version
				>>= \assessor ->
					assessor @: 
					 ("Bug report assessment",
					  requestConfirmationAbout
					  	"Is this bug really critical?" report)
				>>= \confirmed -> if confirmed
						(quickResolveBug bugnr)
						(resolveBug bugnr)
			_ 
				= resolveBug bugnr
where		
	enterInitialReport :: Task BugReport
	enterInitialReport
		= enterInformation "Please describe the bug you have found"
		
	fileBugReport :: BugReport -> Task BugNr
	fileBugReport report
		=	dbCreateItem -&&- getCurrentUser
		>>= \(bug,(uid,name)) ->
			dbUpdateItem {bug & report = report, reportedBy = uid}
		>>| return bug.bugNr 

selectDeveloper :: (DBRef Application) (Maybe String) -> Task UserId
selectDeveloper application version
	=	dbReadItem application
	>>= \mbapp -> case mbapp of
		Nothing
			= getCurrentUser >>= \(uid,name) -> return uid
		Just app
			= selectLeastBusy app.developers
where
	selectLeastBusy :: [UserId] -> Task UserId
	selectLeastBusy []
		=	getCurrentUser >>= \(uid,name) -> return uid
	selectLeastBusy uids
		= 	allTasks [getNumTasksForUser uid \\ uid <- uids]
		>>= \activity -> 
			return (snd (minimum (zip (activity,uids))))
	where	
		minimum l = foldl min (hd l) (tl l) 
		
	getNumTasksForUser :: UserId -> Task Int
	getNumTasksForUser uid = return 42			//TODO: Use API function
	
	 
resolveBug :: BugNr -> Task Void
resolveBug bugnr = showMessage "You are done"

quickResolveBug :: BugNr -> Task Void
quickResolveBug bugnr = showMessage "You are done"

