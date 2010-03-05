implementation module SharedValues

import iTasks, CommonDomain, Text

derive bimap Maybe, (,)

quitButton = ButtonAction (ActionQuit, Always)

//Text-Lines Examples
noteEditor = editor {editorFrom = (\txt -> Note txt),		editorTo = (\(Note txt) _ -> txt)}
listEditor = editor {editorFrom = (\txt -> split "\n" txt),	editorTo = (\l _ -> join "\n" l)}

trimAction = ActionLabel "Trim"

linesPar :: Task Void
linesPar =
				createShared ""
	>>= \sid.	ignoreResult (noteE sid -||- ignoreResult (updateShared "Lines" [quitButton] sid [listEditor]))
where
	noteE sid = 
							updateShared "Text" [ButtonAction (trimAction, Always), quitButton] sid [noteEditor]
		>>= \(action,txt).	case action of
								trimAction	=			setShared sid (trim txt)
												>>|		noteE sid
								_			= 			stop

linesSingle :: Task Void
linesSingle = ignoreResult (updateSharedLocal "Text & Lines" [quitButton] "" [noteEditor,listEditor])

//Calculate Sum Example
calculateSum :: Task Void
calculateSum = ignoreResult (updateSharedLocal "Sum" [quitButton] (0,0) [idEditor, listener {listenerFrom = (\(x,y) -> x + y)}])

//Tree Example
:: Tree a = Leaf | Node (Node a)
:: Node a = { rightChildren	:: Tree a
			, value			:: a
			, leftChildren	:: Tree a
			}
derive gPrint Tree, Node
derive gParse Tree, Node
derive gVisualize Tree, Node
derive gUpdate Tree, Node

toTree :: [a] -> (Tree a) | Ord a
toTree list = makeTree (sort list)
where
	makeTree :: [a] -> (Tree a)
	makeTree []			= Leaf
	makeTree [el:[]]	= Node {rightChildren = Leaf, value = el, leftChildren = Leaf}
	makeTree list		= Node {rightChildren = makeTree end, value = middle, leftChildren = makeTree begin}
	where
		middlePos	= (length list) / 2
		begin		= take (middlePos) list
		middle		= list !! (middlePos) 
		end			= drop (middlePos + 1) list

tree :: Task Void
tree = ignoreResult (updateSharedLocal "List & Balanced Binary Tree" [quitButton] emptyL [idEditor, listener {listenerFrom = toTree}])
where
	emptyL :: [Int]
	emptyL = []

//Merge Test
mergeTest :: Task Void
mergeTest =
				getCurrentUser
	>>= \user.	createShared emptyL			
	>>= \sid.	ignoreResult ((user @: ("1st View", view sid)) -||- (user @: ("2nd View", view sid)))
where
	view sid = updateShared "List" [quitButton] sid [idEditor]
	
	emptyL :: [String]
	emptyL = []

import StdMaybe, GeoDomain

sharedValueExamples :: [Workflow]
sharedValueExamples =	[ workflow "Examples/Shared Values/Text-Lines (parallel tasks)" linesPar
						, workflow "Examples/Shared Values/Text-Lines (single editor)" linesSingle
						, workflow "Examples/Shared Values/Calculate Sum" calculateSum
						, workflow "Examples/Shared Values/Balanced Binary Tree" tree
						, workflow "Examples/Shared Values/Merge Test" mergeTest
						]