implementation module BugReport

import iTasks
import CommonDomain

:: BugReport =
	{ application	:: String
	, version		:: Maybe String
	, yourName		:: String
	, date			:: Date
	, occursAt		:: BugOccurance
	, severity		:: BugSeverity
	, description	:: Note
	, attachment	:: [Document]
	}

:: BugSeverity	= Low | Medium | High | Critical	
:: BugOccurance	= Startup | Shutdown | Other Note

:: Bug =
	{ bugNr			:: BugNr
	, status		:: BugStatus
	, reportedAt	:: (Date,Time)
	, reportedBy	:: UserId
	, report		:: BugReport
	, analysis		:: Maybe BugAnalysis
	}

:: BugStatus = Reported | Assigned UserId | Fixed

:: BugAnalysis =
	{ cause				:: Note
	, affectedVersions	:: [String]
	}

:: BugNr :== Int

derive gPrint     BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis	
derive gParse	  BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis
derive gVisualize BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis
derive gUpdate	  BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis
	
derive bimap (,), Maybe
	
instance DB Bug where
	databaseId					= mkDBid "Bug"
	getItemId bug=:{bugNr}		= DBRef bugNr
	setItemId (DBRef bugNr) bug	= {bug & bugNr = bugNr}

bugReportExample :: [Workflow]
bugReportExample
	= [ workflow "Examples/Miscellaneous/Bug report (simple)" reportBugSimple
	  , {Workflow|name = "Examples/Miscellaneous/Bug report (simple 2)",label = "Bug report (simple 2)",roles = [], mainTask =  bugReport}
	  , workflow "Examples/Miscellaneous/Bug report (advanced)" reportBug
	  ]
	 
reportBugSimple :: Task BugReport
reportBugSimple
	=	enterInformation "Please describe the bug you have found"
	>>=	\report ->
		assignByName "bas" "Bug fix" NormalPriority Nothing
			(showMessageAbout "The following bug has been reported, please fix it." report)
	>>| return report

//Different variant of simple reportBug
bugReport :: Task Void
bugReport = reportBug >>= fixBug
where
	reportBug :: Task BugReport
	reportBug = enterInformation "Please describe the you found"
	
	fixBug :: BugReport -> Task Void
	fixBug bug = "bas" @: ("Bugfix", showMessageAbout "Please fix the following bug" bug)

//Main workflow	  
reportBug :: Task Void
reportBug
	=	enterBugReport
	>>= \report ->
		fileBug report
	>>= \bug ->
		case report.severity of
			Critical 
				= (confirmCritical bug.report
					>>= \critical ->
					assignBug bug critical)
			_
				=	assignBug bug False

assignBug :: Bug Bool -> Task Void
assignBug bug critical
	=	selectDeveloper bug.report.BugReport.application
	>>=	\developer ->
		updateBug (\b -> {Bug| b & status = Assigned developer}) bug
	>>= \bug ->
		assign developer priority Nothing
			(subject @>> resolveBug bug critical)
where
	priority = if critical HighPriority NormalPriority
	subject  = if critical "Critical bug!" "Bug"

resolveBug :: Bug Bool -> Task Void
resolveBug bug critical
	=	analyzeBug bug
	>>= \bug ->
		developBugFix bug
	>>| if critical
		( makePatches bug -&&- mergeFixInMainLine bug
		  >>| wrapUp bug)
		( mergeFixInMainLine bug
		  >>| wrapUp bug)

wrapUp :: Bug -> Task Void
wrapUp bug
	=	updateBug (\b -> {Bug| b & status = Fixed}) bug
	>>= \bug ->
		notifyReporter bug

//Sub tasks

enterBugReport :: Task BugReport
enterBugReport
	=	enterInformation "Please describe the bug you have found"
	
fileBug :: BugReport -> Task Bug
fileBug report
	=	dbCreateItem -&&- getCurrentUser
	>>= \(bug,user) ->
		dbUpdateItem {bug & report = report, reportedBy = user.User.userId}

updateBug :: (Bug -> Bug) Bug -> Task Bug
updateBug f bug = dbUpdateItem (f bug)

confirmCritical :: BugReport -> Task Bool
confirmCritical report
	=	selectDeveloper report.BugReport.application
	>>= \assessor ->
		assign assessor HighPriority Nothing
			( "Bug report assessment"
			  @>>
			  requestConfirmationAbout "Is this bug really critical?" report
			)
	
selectDeveloper :: String -> Task UserId
selectDeveloper application
	=	findAppDevelopers application
	>>= \developers -> case developers of
		[]	= getCurrentUser >>= \user -> return user.User.userId
		_	= selectLeastBusy developers
where
	findAppDevelopers :: String -> Task [UserId]
	findAppDevelopers "itasks"	= return [0]
	findAppDevelopers _			= return []
		
	selectLeastBusy :: [UserId] -> Task UserId
	selectLeastBusy []
		=	getCurrentUser >>= \user -> return user.User.userId
	selectLeastBusy uids
		= 	allTasks [getNumTasksForUser uid \\ uid <- uids]
		>>= \activity -> 
			return (snd (minimum (zip (activity,uids))))
	where	
		minimum l = foldl min (hd l) (tl l) 
		
	getNumTasksForUser :: UserId -> Task Int
	getNumTasksForUser uid = return 42			//TODO: Use API function
	 
analyzeBug :: Bug -> Task Bug
analyzeBug bug
	=	determineCause bug 
	>>=	\cause ->
		dbUpdateItem {bug & analysis = Just {cause = cause, affectedVersions = []}}
where
	determineCause bug
		= enterInformationAbout "What is the cause of the following bug?" bug
		
developBugFix :: Bug -> Task Void
developBugFix bug = showMessageAbout "Please implement a fix for the following bug:" bug

mergeFixInMainLine :: Bug -> Task Void
mergeFixInMainLine bug = showMessageAbout "Please merge the bugfix in the main line of version control" bug

makePatches :: Bug -> Task Void
makePatches bug =
	case bug.analysis of
		Nothing
			= return Void
		Just {affectedVersions = []}
			= return Void
		Just {affectedVersions = versions}
			= allTasks [showMessageAbout ("Please make a patch of bugfix " <+++ bug.bugNr <+++
								" for the following version of " <+++ bug.Bug.report.application)
								version
						\\ version <- versions
					   ]
			  >>| return Void
		
notifyReporter :: Bug -> Task Void
notifyReporter bug = notifyUser "The bug you reported has been fixed" bug.reportedBy
