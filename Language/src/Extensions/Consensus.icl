implementation module Consensus

import iTasks, GenEq
import CommonDomain, Messaging, Groups

derive bimap Maybe, (,)



//========================================================================================================================================================================
// DatePicker
//========================================================================================================================================================================

:: DateVotes :== (HtmlDisplay [DateVote])

:: DateVote = 
	{ date :: Date
	, vote :: (Editable Vote)
	}

:: Vote = Yes | No | Maybe

:: VoteCount =
	{ date 	:: Date
	, yes 	:: Int
	, no 	:: Int
	, maybe :: Int
	}
	
derive class iTask	DateVote, Vote, VoteCount
derive gEq			Date

pickADate :: Task Void
pickADate = enterInformation "Subject" "What is the subject?"
	>>= \subj ->	getCurrentUser
	>>= \me -> 		updateInformation "Manager" "Who should be managing the decision?" me
	>>= \ref ->		enterInformation "Choose people" "Who else should be involved in the decision?"
	>>= \oth ->		(ref @: datePicker [ref:oth])
	>>= \date ->	broadcast [ref:oth] ("The chosen date for "+++subj+++": ") (Just date)

datePicker :: [User] -> Task Date
datePicker users =  pickDates
	>>= \dates -> allProc [u @>> voteDates dates \\ u <- users] Open
	>>= \votes -> pickFinal votes
where
	pickDates :: Task [Date]
	pickDates = enterInformation "Pick dates" "Please select date options"

	voteDates :: [Date] -> Task DateVotes
	voteDates dates = updateInformation "Date preference" "Please indicate your preference" (HtmlDisplay [{DateVote | date = d, vote = (Editable Maybe)} \\ d <- dates])

	pickFinal :: [DateVotes] -> Task Date
	pickFinal votes
		# v = (map fromHtmlDisplay votes)
		# init = [{VoteCount | date = dv.DateVote.date, yes = 0, no = 0, maybe = 0} \\ dv <- (hd v)]
		# overview = foldl countVotes init v
		=   enterChoice "Final decision" "Please select the final option" overview
		>>= \final -> return final.VoteCount.date
	where
		countVotes votecount [] = votecount
		countVotes votecount [d:ds]
			# votecount = [if(d.DateVote.date === vc.VoteCount.date) (updateCount vc d.vote) vc \\ vc <- votecount]
			= countVotes votecount ds
			
		updateCount vc=:{yes,no,maybe} (Editable v)
			= case v of
				Yes 	= {VoteCount|vc & yes 		= inc yes}
				No  	= {VoteCount|vc & no  		= inc no}
				Maybe	= {VoteCount|vc & maybe 	= inc maybe}
