implementation module BugReport

import iTasks
import CommonDomain

:: BugReport =
	{ application	:: String
	, version		:: Maybe String
	, priority		:: BugPriority
	, description	:: Note
	}

:: BugPriority = Low | Medium | High | Critical


derive gPrint		BugReport, BugPriority
derive gParse		BugReport, BugPriority
derive gVisualize	BugReport, BugPriority
derive gUpdate		BugReport, BugPriority

bugReportExample :: [Workflow]
bugReportExample = [workflow "Examples/Miscellaneous/Bug report" reportBug]

reportBug :: Task Void
reportBug
	=	fileReport
	>>= \initialReport ->
		assessReport initialReport

fileReport :: Task BugReport
fileReport
	= enterInformation "Please describe the bug you have found"
		
assessReport :: BugReport -> Task Void
assessReport report = return Void