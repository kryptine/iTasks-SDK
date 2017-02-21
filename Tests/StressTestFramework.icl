implementation module StressTestFramework
import iTasks, iTasks.UI.Definition, Text.HTML, iTasks._Framework.Serialization
import Text.JSON, StressTestFramework
import qualified Internet.HTTP
from Internet.HTTP import :: HTTPResponse(..)
import qualified Text.URI
import qualified Text
from Text import class Text, instance Text String
from StdEnv import flip
from iTasks.UI.Editor.Builtin import :: ChoiceNode(..)
import iTasks.API.Extensions.SVG.SVGEditor
from Data.List import maximum

TEST_SERVER_PORT :== "12345"

stest :: String String (Task a) ([ActionWithTaskId] -> TestStepEvent) -> StressTest | iTask a
stest name description tut step =
    {name = name, description = description, testStep = step, taskUnderTest = tut @! ()}

:: RunParameters = { numberOfSteps :: Int }

derive class iTask StressTest, TestStepEvent, HTTPResponse, RunParameters

runStressTests :: [StressTestSuite] -> Task ()
runStressTests suites =
    ( editSelection (Title "Select test") False (SelectInTree toTree selectTest) suites [] @? tvHd
    >&> withSelection (viewInformation () [] "Select a test") testStress) <<@ ArrangeWithSideBar 0 LeftSide 250 True @! ()
where
    testStress (test=:{StressTest|name, description}, suiteName) =
             (viewInformation () [] (H1Tag [] [Text name]) <<@ ApplyLayout (setAttributes (heightAttr WrapSize)))
        -&&- catchAll
                 ( viewInformation "Test description" [] description ||- updateInformation "Test run parameters" [] {numberOfSteps = 100}
                   >>* [OnAction (Action "Start test") (hasValue (runTest suiteName test))] @! ()
                 )
                 (\e -> viewInformation "Error" [] e @! ())

    runTest suiteName {StressTest|name, testStep} {numberOfSteps} =
        (     (runTestServer <<@ ApplyLayout (hideSubs SelectRoot))
          ||- (sleep "Waiting for test server..." 2 >>| performRequests)
        ) >>=
        visualizeResults
    where
        performRequests =
            startSession uri >>= \(instanceNo, actions) ->
            performSteps instanceNo testStep actions 0 [] >>= \result ->
            shutdownTestServer >>|
            return result

        performSteps :: Int ([ActionWithTaskId] -> TestStepEvent) [ActionWithTaskId] Int [Int] -> Task [Int]
        performSteps _ _ _ n acc | n == numberOfSteps = return (reverse acc)
        performSteps instanceNo step actions n acc =
            (     viewInformation "Progress" [] ((n +++> "/") <+++ numberOfSteps)
              ||- case step actions of
                      DoAction action ->
                          getTimeMs >>= \tBefore ->
                          doAction uri instanceNo action >>= \actions ->
                          getTimeMs >>= \tAfter ->
                          return (actions, tAfter - tBefore)
            ) >>= \(actions, respTime) ->
            performSteps instanceNo step actions (inc n) [respTime:acc]

        uri = testURI suiteName name

    selectTest suites [idx] 
		| idx >= 0  = [(flatten [[(t,name) \\ t <- tests] \\ {name,tests} <- suites]) !! idx]
		| otherwise = []
	selectTest _ _ = []

    toTree suites = reverse (snd (foldl addSuite (0,[]) suites))

    addSuite (i,t) {StressTestSuite|name,tests}
		| isEmpty [t \\ t <- tests]  = (i,t) //There are no interactive tests in the suite
		# (i,children) = foldl addTest (i,[]) tests
		= (i, [{ChoiceNode|id = -1 * i, label=name, expanded=False, icon=Nothing, children=reverse children}:t])

    addTest (i,t) {StressTest|name}
		= (i + 1, [{ChoiceNode|id = i, label=name, expanded=False, icon=Nothing, children=[]}:t])
	addTest (i,t) _ = (i,t)

exposedStressTestTasks :: [StressTestSuite] -> [PublishedTask]
exposedStressTestTasks suites = flatten [suiteTasks name tests \\ {name,tests} <- suites]
where
    suiteTasks suiteName tests =
        [publish (testURL suiteName name) (const taskUnderTest) \\ {name,taskUnderTest} <- tests]

// utility

visualizeResults :: [Int] -> Task ()
visualizeResults res = viewInformation "Results" [ViewUsing id (fromSVGEditor svgeditor)] ()
where
	svgeditor = {SVGEditor|initView=const (),renderImage = \_ _ _ -> resultViz, updView = \m v -> v, updModel = \m v -> m}

    resultViz = overlay`
        [(AtMiddleX, AtMiddleY), (AtMiddleX, AtMiddleY)]
        []
        [empty (width + margin*.2) (height + margin*.2), resultViz`]
        NoHost

    resultViz` = overlay`
        [(AtLeft, AtTop)]
        [(zero, height), (zero, zero), (zero, height - tickLength /. 2), (~tickLength /. 2, zero), (zero, zero)]
        [xAxis, yAxis, xTicks, yTicks, resLine]
        NoHost

    // coordinate system
    xAxis = line Nothing Slash width zero
    yAxis = line Nothing Slash zero height
    xTicks = beside`
        []
        [(iStep *. resultIdxAtNthTick j, zero) \\ j <- [0..nXTicks-1]]
        [xTick (resultIdxAtNthTick j + 1) \\ j <- [0..nXTicks-1]]
        NoHost
    where
        resultIdxAtNthTick n = n * (nRes - 1) / (nXTicks - 1)

        xTick i = line (Just {defaultMarkers & markerStart = Just label}) Slash zero tickLength
        where
            label =  (text ticksLabelFont (toString i +++ " "))
        nXTicks = 10

    yTicks = above`
        []
        [(zero, yStep *. j) \\ j <- [0..nYTicks-1]]
        [yTick ((nYTicks-1-j) * maxRes / (nYTicks-1)) \\ j <- [0..nYTicks-1]]
        NoHost
    where
        yStep = height /. (nYTicks - 1)

        yTick i = line (Just {defaultMarkers & markerStart = Just label}) Slash tickLength zero
        where
            label = text ticksLabelFont (toString i +++ "ms ")
        nYTicks = 10

    tickLength = px 10.0
    ticksLabelFont = normalFontDef "Times New Roman" 12.0

    // actual result lines
    resLine = polyline Nothing [(iStep *. i, height - tStep *. t) \\ i <- [0..] & t <- res]
              <@< {stroke = toSVGColor "red"}
    
    iStep  = width  /. (nRes - 1)
    tStep  = height /. maxRes
    maxRes = maximum res
    width  = px 800.0
    height = px 500.0
    margin = px  50.0
    nRes   = length res

startSession :: URI -> Task (Int, [ActionWithTaskId])
startSession uri =
    sendToTestServer uri (JSONString "new") >>= \rsp ->
    case rsp of
        JSONObject [("instanceNo", JSONInt instanceNo), ("instanceKey", JSONString _), ("ui", ui)] ->
            return (instanceNo, possibleActions ui)
        _ ->
            throw ("Unexpected response from test server: " +++ toString rsp)

doAction :: URI Int ActionWithTaskId -> Task [ActionWithTaskId]
doAction uri instanceNo (taskId, Action actionId) =
    sendToTestServer
        uri
        ( JSONArray [ JSONString "event"
                    , JSONInt instanceNo
                    , JSONArray [JSONString taskId,JSONNull,JSONString actionId]
                    ]
        ) >>= \rsp ->
        return (possibleActions rsp)

shutdownTestServer :: Task ()
shutdownTestServer = startSession uri @! ()
where
    uri = fromJust ('Text.URI'.parseURI
            ('Text'.concat ["http://localhost:", TEST_SERVER_PORT, "/shutdown/gui-http"])
        )

sendToTestServer :: URI JSONNode -> Task JSONNode
sendToTestServer uri msg = callHTTP
    'Internet.HTTP'.HTTP_POST
    uri
    (toString msg)
    Ok @ \rsp -> fromString rsp.rsp_data

testURI :: String String -> URI
testURI suiteName testName = fromJust ('Text.URI'.parseURI
        ('Text'.concat ["http://localhost:", TEST_SERVER_PORT, testURL suiteName testName, "/gui-http"])
    )

testURL :: String String -> String
testURL suiteName testName = 'Text'.concat ["/stress/", suiteName`, "/", testName`]
where
    suiteName` = 'Text'.replaceSubString " " "" suiteName
    testName`  = 'Text'.replaceSubString " " "" testName

runTestServer =
    worldIO determineAppPathErr >>= \exePath ->
    callProcess "Test iTasks Server" [] exePath ["-port", TEST_SERVER_PORT] Nothing

determineAppPathErr :: *World -> *(!MaybeError String FilePath, !*World)
determineAppPathErr w = let (r,w`) = determineAppPath w in (Ok r, w`)

possibleActions :: JSONNode -> [ActionWithTaskId]
possibleActions ui = possibleActions` ui []
where
    possibleActions` (JSONObject fields) acc = case filter (\(k,_) -> k == "actionId" || k == "taskId") fields of
        [("actionId", JSONString actionId), ("taskId", JSONString taskId)] -> [(taskId, Action actionId) : acc]
        [("taskId", JSONString taskId), ("actionId", JSONString actionId)] -> [(taskId, Action actionId) : acc]
        _ -> foldl (\acc (_, json) -> possibleActions` json acc) acc fields
    possibleActions` (JSONArray fields) acc = foldl (flip possibleActions`) acc fields
    possibleActions` _ acc = acc

sleep :: d Int -> Task () | toPrompt d
sleep d delta =
    get currentTimestamp >>= \(Timestamp t) ->
    wait d (\(Timestamp t`) -> t` >= t + delta) currentTimestamp @! ()

getTimeMs :: Task Int
getTimeMs = worldIO getTimeMs`
where
    getTimeMs` :: *World -> *(!MaybeError Bool Int,!*World)
    getTimeMs` w 
        #(Clock c, w`) = clock w
        = (Ok (c / 1000), w`)

