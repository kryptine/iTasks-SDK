implementation module Vote

//This example lets users give a vote.
//Only the user with role "root" can view the votes
 
import iTasks
import CommonDomain

derive gPrint 		Vote
derive gParse		Vote
derive gVisualize	Vote
derive gUpdate		Vote
derive bimap (,), Maybe

:: Vote = { userName	:: UserName
		  , vote		:: Int
		  , comment		:: String
		  }

voteExample :: [Workflow]
voteExample = [{ name		= "Examples/Business/Vote/Show votes"
			   , label		= "Show votes"
			   , roles		= ["root"]
			   , mainTask	= showVotes
			   }
			  ,{ name		= "Examples/Business/Vote/New vote"
			   , label		= "Give vote"
			   , roles		= []
			   , mainTask	= giveVote
			   }
			  ,{ name		= "Examples/Business/Vote/New comment"
			   , label		= "Give comment"
			   , roles		= []
			   , mainTask	= giveComment
			   }
			  ]

showVotes :: Task Void
showVotes
=  						readVotesDB
	>>=	\votes 		-> 	getDisplayNames [vote.Vote.userName \\ vote <- votes]
	>>= \userNames	->	showMessage
							[ Text "The following votes are given:", BrTag [], BrTag []
							, formatVotes	[(toString i) 					\\ i <- [0..10]]
											[(toString (number i votes))	\\ i <- [0..10]]
							, HrTag []
							, formatComments [(u,v) \\ v <- votes & u <- userNames]
							, HrTag []
							]
where	
	number i votes = length [n \\ n <- votes | n.vote == i]
	formatVotes	header data	= TableTag	[StyleAttr "border: 1px solid #ccc"]
										[TrTag [] [ThTag [] [BTag [] [Text th]] \\th <- header]
										,TrTag [] [TdTag [] [Text td] \\ td <- data]
										]
	formatComments rows		= TableTag	[]
										[TrTag [] [TdTag [] [Text u,Text ":"],TdTag [] [Text v.comment]] \\ (u,v) <- rows]
	
giveVote :: Task Void
giveVote 
=							getCurrentUser
	>>= \currentUser ->		readMyVoteDB (toUserName currentUser)
	>>= \(vote,comment) ->	enterChoice
								[ Text ("Previous vote given:" +++ if (vote == -1) "No vote given" (toString vote)), BrTag [], BrTag []
								, Text "Give your new vote (0 = lowest, 10 = highest)"
								] [0..10]
	>>= \vote -> 			readMyVoteDB (toUserName currentUser)
	>>= \(_,comment) ->		writeVotesDB {userName = toUserName currentUser, vote = vote, comment = comment}
	>>|						showMessage [Text ("Your vote " +++ toString vote +++ " has been stored!")]

giveComment :: Task Void
giveComment
=							getCurrentUser
 	>>= \currentUser ->		readMyVoteDB (toUserName currentUser)
	>>= \(vote,comment) ->	enterInformation
								[ Text "Previous comment given:", BrTag [], BrTag []
								, Text (if (comment == "" ) "None" comment), BrTag [], BrTag []
								, Text "Submit a new comment:", BrTag [], BrTag []
								] 
	>>= \(Note comment) -> readMyVoteDB (toUserName currentUser)
	>>= \(vote,_) ->		
							writeVotesDB {userName = toUserName currentUser, vote = vote, comment = comment}
	>>|						showMessage
								[ Text "Your comment:", BrTag [], BrTag []
								, Text comment, BrTag [], BrTag []
								, Text "has been stored!"
								]

//Simple votes database

votesId :: DBid [Vote]
votesId	= mkDBid "votes"

readVotesDB :: Task [Vote]
readVotesDB = readDB votesId

readMyVoteDB :: UserName -> Task (Int,String)
readMyVoteDB name
=					readVotesDB
	>>=	\votes ->	return 	(case (filter (\vote -> vote.Vote.userName == name) votes) of 
									[] 			-> (-1,"")
									[vote:_] 	-> (vote.vote, vote.comment)
								)

writeVotesDB :: Vote -> Task [Vote]
writeVotesDB acc
=	readVotesDB
	>>= \accs -> writeDB votesId [acc:[oacc \\ oacc <- accs | oacc.Vote.userName <> acc.Vote.userName]] 

