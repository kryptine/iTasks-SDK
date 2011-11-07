implementation module ESMVizTool

import iTasks
//import StdListExtensions
import ESMSpec
import GenPrint
//import _SystemArray
//from _SystemArray import qualified class Array(..)

import Graphviz
import GraphvizVisualization

derive bimap (,), Maybe
derive class iTask	KnownAutomaton, State
/*
derive gVisualize 	(,,,,), (,,,,,)
derive gUpdate	 	(,,,,), (,,,,,)
derive gVerify		(,,,,), (,,,,,)
*/
//derive class iTask (,,,,)
//derive class iTask (,,,,,)

getDefaultValue :: t | ggen{|*|} t
getDefaultValue = ggen{|*|} 2 aStream !! 0

//:: State s i o :== (KnownAutomaton s i o,[s],Traces s i o,Int,Int)
:: State s i o
 =	{ ka	:: !KnownAutomaton s i o
	, ss	:: ![s]
	, trace	:: !Traces s i o
	, n		:: !Int
	, r		:: !Int
	}

esmVizTool :: !(ESM s i o) *World -> *World
			| all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
esmVizTool esm world
	// = startEngine (manageWorkflows [workflow "ESM Viz Tool" "ESM visualization" (iterateTask (DiGraphFlow esm) newstate)]) world
	= startEngine (iterateTask (DiGraphFlow esm) newstate) world
where
	newstate = { ka = newKA, ss = [esm.s_0], trace = [], n = 1, r = 20080929}
	 
DiGraphFlow :: !(ESM s i o) (State s i o) -> Task (State s i o) 
				| all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
DiGraphFlow	esm st=:{ka,ss,trace,n,r}
 =	anyTask	[ state esm st
 			, chooseTask "Choose an input... " (sortBy (\(a,_) (b,_).a<b) [(render i,step esm st i) \\ i<-inputs])
			, chooseTask "Do one of the following actions..."
				[("Back" , back st)
				,("Prune", prune st)
				,("Reset", return newState) //(newKA,[esm.s_0],[],1,rn))
				,("Clear trace", return {st & trace  = []}) //(ka,as,[],n,rn))
				,("Random input",if (isEmpty newInputs)
 									(if (isEmpty inputs2)
 										(return st)
 										(step esm st (inputs2!!((abs r) rem (length inputs2)))))
 									(step esm st (newInputs!!((abs r) rem (length newInputs)))) )
				]
    		, stepN esm st
//    		, getDefaultValue >>= showStickyMessage "Trace" (traceHtml trace)
// XXX    		, getDefaultValue >>= viewInformation ("Trace", traceHtml trace) [] 
    		]
where
	inputs		= possibleInputs esm ss
	inputs2		= [ i \\ i <- inputs, (s,j,_,t) <- ka.trans | i === j && gisMember s ss && ~ (gisMember t ss) ]
	newInputs	= filter (\i.not (gisMember i usedInputs)) inputs
	usedInputs	= [ j \\ (s,j,_,_) <- ka.trans | gisMember s ss ]
	rn			= hd (genRandInt r)
	newState 	= { ka = newKA, ss = [esm.s_0], trace = [], n = 1, r = rn}


stepN :: !(ESM s i o) !(State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
stepN esm st=:{ka,ss,trace,n,r}
// =		updateInformation "Add multiple" "Or add multiple steps..." n
 =		updateInformation "Add multiple steps..." [] n
	>>= doStepN esm st
	
doStepN :: !(ESM s i o) !(State s i o) Int -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
doStepN esm state=:{ka,ss,trace,r} n
	= if (n>0)
  		(return {state & ka = addTransitions n esm ss (possibleInputs esm ss) ka, r = rn }) //(addTransitions n esm as (possibleInputs esm as) ka,as,trace,n,rn)) 
  		(return {state & n = 1}) //(ka,as,trace,1,rn))
where rn = hd (genRandInt r)

//buttonTask msg task = enterChoice "" "" [task <<@ Subject msg] >>= id
//chooseTask msg tasks = enterChoice msg msg [t <<@ Subject l \\ (l,t) <- tasks] >>= id
chooseTask :: !d ![(String,Task o)] -> Task o | descr d & iTask o 
chooseTask msg tasks = enterChoice msg [] [l \\ (l,t) <- tasks] >>= \c . hd [t \\ (l,t) <- tasks | l == c]

prune :: !(State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
prune state=:{ka,ss,trace,n,r}
	= return {state & ka = {ka &trans=[t\\t<-ka.trans|gisMember t onTraces],issues=[i\\i=:(t,_)<-ka.issues|gisMember t onTraces]}, r = rn}
where
	onTraces = flatten trace
	rn		= hd (genRandInt r)

state :: !(ESM s i o) !(State s i o) -> Task (State s i o) | all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
state esm st=:{ka,ss,trace,n,r}
	| isEmpty ka.issues
		=	digraph
		=	viewInformation ("Issues",issuesToHtml ka.issues) [] Void ||- digraph
where
	digraph = updateInformation "ESM" [UpdateView (GetLocal toView,SetLocal fromView)] st
	
	toView st 
		= length st.trace
		//= "TODO SHOW VISUALIZATION OF ESM STATE"
	fromView v st _ = st

	 //(mkDigraph "ESM" (ka, esm.s_0, ss, allEdgesFound esm ka, sharedNodesOf ka, map fst ka.issues, flatten trace))
	 //	>>= updateDig st

updateDig :: !(State s i o) !String  -> Task (State s i o) | all, Eq, genShow{|*|} s & all, ggen{|*|} i & all o
updateDig state=:{ka,ss,trace,n,r} label
	#(ss`,trace`)	= findSelectedStates label ka ss trace
	# r`			= hd (genRandInt r)
	= return {state & ss = ss`, trace = trace`, r = r`} //(ka,as`,trace`,n,r`)
where
	findSelectedStates label ka ss trace
		# ss` = take 1 [s \\ s <- nodesOf ka | show1 s == label]
		= case ss` of
				[]	 = (ss,trace)
				[ns] | gisMember ns ss
						= (ss`,narrowTraces trace ss`)
					 # oneStep = [tr \\ tr=:(s,i,o,t)<-ka.trans | t===ns && gisMember s ss]
					 | not (isEmpty oneStep)
					 	= (ss`,trace++[oneStep])
						= (ss`,partTraces trace ns [])
					 = (ss`,[]) // ??

step :: !(ESM s i o) (State s i o) i -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
step esm state=:{ka,ss,trace,n,r} i
		= let next   = nextStates esm i ss
			  ka`    = addTransitions 1 esm ss [i] ka
			  trace` = addStep esm ss i trace
			  rn	 = hd (genRandInt r)
		  in return {state & ka = ka`, trace = trace`} //(ka`,next,trace`,n,rn)

back :: (State s i o) -> Task (State s i o) | all, Eq s & all, ggen{|*|} i & all o
back state=:{ka,ss,trace,n,r}
	| isEmpty trace
		= return {state & r = hd (genRandInt r)} //(ka,as,[],n,hd (genRandInt r))
		= let next   = startStates (last trace)
			  trace` = init trace
			  rn	 = hd (genRandInt r)
		  in return {state & trace = trace`} //(ka,next,trace`,n,rn)

newKA = {trans=[],issues=[]}

iterateTask :: (a->Task a) a -> Task a | iTask a
iterateTask task a = task a >>= iterateTask task

orTaskL :: [Task a] -> Task a | iTask a
orTaskL l = foldl1 (-||-) l

foldl1 op [a]   = a
foldl1 op [a:x] = op a (foldl1 op x)

traceHtml trace
   = [H3Tag [] [Text "Trace:"]
     ,TableTag []
       [TrTag [] [TdTag [] (map Text (flatten [transToStrings t [] \\ t <- step]))]
           \\ step <- trace
       ]
     , BrTag []
     ]

issuesToHtml :: [(SeenTrans s i o,[String])] -> [HtmlTag] | render s & render i & render o
issuesToHtml [] = []
issuesToHtml l
	=	[H3Tag [] [Text "Issues found:"]
		:	[ TableTag []
				[ TrTag [] [TdTag [] (map Text (transToStrings t [": ":ss])) ]
				\\ (t,ss) <- l
				]
			]
		]

transToStrings :: (SeenTrans s i o) [String] -> [String] | render s & render i & render o
transToStrings (s,i,o,t) c = ["(",render s,",",render i,",[":showList "," o ["],",render t,")":c]]

showList :: !String ![a] [String] -> [String] | render a
showList delimit []    c = c
showList delimit [x]   c = [render x:c]
showList delimit [x:r] c = [render x,delimit:showList delimit r c]

partTraces :: (Traces s i o) s (Traces s i o) -> (Traces s i o) | gEq{|*|} s
partTraces [] s seen = []
partTraces [trans:rest] s seen
	| gisMember s (targetStates trans)
		= narrowTraces (reverse [trans:seen]) [s]
		= partTraces rest s [trans:seen]

allEdgesFound :: (ESM s i o) (KnownAutomaton s i o) -> [s] | gEq{|*|} s & ggen{|*|} i
allEdgesFound esm automaton
	= [s \\ s <- nodesOf automaton
	      | length (edgesFrom s automaton) == length [t \\ i<-enumerate,t<-nextStates esm i [s]] 
	  ]

toHtmlString :: a -> String | gVisualizeText{|*|} a
toHtmlString x
	# string = visualizeAsText AsDisplay x
	= toString [checkChar c \\ c <-fromString string]
//	= {checkChar c \\ c <-: string}
where
	checkChar '"' = '\''
	checkChar  c  = c

instance render Int where render i = toString i
