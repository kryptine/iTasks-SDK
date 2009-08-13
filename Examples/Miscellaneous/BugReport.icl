implementation module BugReport

import iTasks
import CommonDomain

:: BugReport =
	{ application	:: String
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
	
:: BugNr :== Int
:: BugSeverity = Low | Medium | High | Critical
:: BugStatus = Reported | Assigned | Reproduced | Resolved

derive gPrint     BugReport, Bug, BugSeverity, BugStatus	
derive gParse	  BugReport, Bug, BugSeverity, BugStatus
derive gVisualize BugReport, Bug, BugSeverity, BugStatus
derive gUpdate	  BugReport, Bug, BugSeverity, BugStatus

instance DB Bug where
	databaseId					= mkDBid "Bugs"
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
				= selectAssessor report.application report.version
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

selectAssessor :: String (Maybe String) -> Task UserId
selectAssessor application version = return 0

resolveBug :: BugNr -> Task Void
resolveBug bugnr = showMessage "You are done"

quickResolveBug :: BugNr -> Task Void
quickResolveBug bugnr = showMessage "You are done"

