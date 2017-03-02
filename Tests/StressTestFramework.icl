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

stestState :: String String (Task a) ([ActionWithTaskId] [EditorId] st -> (TestStepEvent, st)) st -> StressTestContainer | iTask a & iTask st
stestState name description tut step initSt = StressTestContainer
    {name = name, description = description, testStep = step, taskUnderTest = tut @! (), initState = initSt}

stest :: String String (Task a) ([ActionWithTaskId] [EditorId] -> TestStepEvent) -> StressTestContainer | iTask a
stest name description tut step = StressTestContainer
    {name = name, description = description, testStep = \a e st -> (step a e, st), taskUnderTest = tut @! (), initState = ()}

tsAction :: ActionWithTaskId -> TestStepEvent
tsAction action = DoAction action

tsEdit :: EditorId v -> TestStepEvent | JSONEncode{|*|} v
tsEdit editorId v = Edit editorId (toJSON v)

:: RunParameters = { numberOfSteps :: Int }

derive class iTask StressTest, TestStepEvent, HTTPResponse, RunParameters

runStressTests :: [StressTestSuite] -> Task ()
runStressTests suites =
    ( editSelection (Title "Select test") False (SelectInTree toTree selectIdx) suites [] @? tvHd
    >&> withSelection (viewInformation () [] "Select a test") testStress) <<@ ArrangeWithSideBar 0 LeftSide 250 True @! ()
where
    //testStress :: Int -> Task ()
    testStress idx =
             (viewInformation () [] (H1Tag [] [Text (testName test)]) <<@ ApplyLayout (setAttributes (heightAttr WrapSize)))
        -&&- catchAll
                 ( viewInformation "Test description" [] (testDescr test) ||- updateInformation "Test run parameters" [] {numberOfSteps = 100}
                   >>* [OnAction (Action "Start test") (ifValue (\{numberOfSteps} -> numberOfSteps > 0) (runTest suiteName test))] @! ()
                 )
                 (\e -> viewInformation "Error" [] e @! ())
    where
        (test, suiteName) = (flatten [[(t,name) \\ t <- tests] \\ {name,tests} <- suites]) !! idx
        testName  (StressTestContainer {StressTest|name})        = name
        testDescr (StressTestContainer {StressTest|description}) = description

    runTest :: String StressTestContainer RunParameters -> Task ()
    runTest suiteName (StressTestContainer {StressTest|name, testStep, initState}) {numberOfSteps} =
        (     (runTestServer <<@ ApplyLayout (hideSubs SelectRoot))
          ||- (sleep "Waiting for test server..." 2 >>| performRequests)
        ) >>=
        visualizeResults
    where
        performRequests =
            startSession uri >>= \(instanceNo, actions, editors) ->
            performSteps instanceNo testStep actions editors initState 0 [] >>= \result ->
            shutdownTestServer >>|
            return result

        performSteps :: Int
                        ([ActionWithTaskId] [EditorId] st -> (TestStepEvent, st))
                        [ActionWithTaskId]
                        [EditorId]
                        st
                        Int
                        [Int]
                     -> Task [Int]
                      | iTask st
        performSteps _ _ _ _ _ n acc | n == numberOfSteps = return (reverse acc)
        performSteps instanceNo step actions editors st n acc =
            (     viewInformation "Progress" [] ((n +++> "/") <+++ numberOfSteps)
              ||- (if (n rem 100 == 0) (sleep "..." 1) (return ())) >>|
                  getTimeMs >>= \tBefore ->
                  let (stepEvent, st`) = step actions editors st in
                  (case stepEvent of
                      DoAction action ->
                          doAction uri instanceNo action
                      Edit editorId value ->
                          edit uri instanceNo editorId value
                  ) >>= \(actions, editors) ->
                  getTimeMs >>= \tAfter ->
                  return (actions, editors, st`, tAfter - tBefore)
            ) >>= \(actions, editors, st`, respTime) ->
            performSteps instanceNo step actions editors st` (inc n) [respTime:acc]

        uri = testURI suiteName name

    selectIdx _ [idx] | idx >= 0 = [idx]
    selectIdx _ _                = []

    toTree suites = reverse (snd (foldl addSuite (0,[]) suites))

    addSuite (i,t) {StressTestSuite|name,tests}
		| isEmpty [t \\ t <- tests]  = (i,t) //There are no interactive tests in the suite
		# (i,children) = foldl addTest (i,[]) tests
		= (i, [{ChoiceNode|id = -1 * i, label=name, expanded=False, icon=Nothing, children=reverse children}:t])

    addTest (i,t) (StressTestContainer {StressTest|name})
		= (i + 1, [{ChoiceNode|id = i, label=name, expanded=False, icon=Nothing, children=[]}:t])
	addTest (i,t) _ = (i,t)

exposedStressTestTasks :: [StressTestSuite] -> [PublishedTask]
exposedStressTestTasks suites = flatten [suiteTasks name tests \\ {name,tests} <- suites]
where
    suiteTasks suiteName tests =
        [publish (testURL suiteName name) (const taskUnderTest) \\ StressTestContainer {name,taskUnderTest} <- tests]

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
    resLine = polyline Nothing ( [  (iStep *. i, height - tStep *. t)
                                 \\ i <- [0..nRes - 1]
                                 &  t <- res
                                 // make sure only maxNrPointsDrawn are drawn
                                 | i rem (divRoundUp nRes maxNrPointsDrawn) == 0
                                 ]
                                 ++
                                 [(iStep *. (nRes - 1), height - tStep *. last res)]
                               )
              <@< {stroke = toSVGColor "red"}
    
    iStep  = width  /. (nRes - 1)
    tStep  = height /. maxRes
    maxRes = maximum res
    width  = px 800.0
    height = px 500.0
    margin = px  50.0
    nRes   = length res
    maxNrPointsDrawn = 400
    divRoundUp x y
        | x rem y == 0 = x / y
        | otherwise    = x / y + 1

startSession :: URI -> Task (Int, [ActionWithTaskId], [EditorId])
startSession uri =
    sendToTestServer uri (JSONString "new") >>= \rsp ->
    case rsp of
        JSONObject [("instanceNo", JSONInt instanceNo), ("instanceKey", JSONString _), ("ui", ui)] ->
            return (instanceNo, possibleActions ui, editors ui)
        _ ->
            throw ("Unexpected response from test server: " +++ toString rsp)

doAction :: URI Int ActionWithTaskId -> Task ([ActionWithTaskId], [EditorId])
doAction uri instanceNo (taskId, Action actionId) =
    sendToTestServer
        uri
        ( JSONArray [ JSONString "event"
                    , JSONInt instanceNo
                    , JSONArray [JSONString taskId,JSONNull,JSONString actionId]
                    ]
        ) >>= \rsp ->
        return (possibleActions rsp, editors rsp)

edit :: URI Int EditorId JSONNode -> Task ([ActionWithTaskId], [EditorId])
edit uri instanceNo (taskId, editorId) value =
    sendToTestServer
        uri
        ( JSONArray [ JSONString "event"
                    , JSONInt instanceNo
                    , JSONArray [JSONString taskId, JSONString editorId, value]
                    ]
        ) >>= \rsp ->
        return (possibleActions rsp, editors rsp)

shutdownTestServer :: Task ()
shutdownTestServer = startSession uri @! ()
where
    uri = fromJust ('Text.URI'.parseURI
            ('Text'.concat ["http://localhost:", TEST_SERVER_PORT, "/shutdown/gui-http"])
        )

sendToTestServer :: URI JSONNode -> Task JSONNode
sendToTestServer uri msg = callHTTP`
    'Internet.HTTP'.HTTP_POST
    uri
    (toString msg)
    Ok @ \{rsp_data} -> fromString rsp_data

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

editors :: JSONNode -> [EditorId]
editors ui = editors` ui []
where
    editors` (JSONObject fields) acc = case filter (\(k,_) -> k == "editorId" || k == "taskId") fields of
        [("editorId", JSONString editorId), ("taskId", JSONString taskId)] -> [(taskId, editorId) : acc]
        [("taskId", JSONString taskId), ("editorId", JSONString editorId)] -> [(taskId, editorId) : acc]
        _ -> foldl (\acc (_, json) -> editors` json acc) acc fields
    editors` (JSONArray fields) acc = foldl (flip editors`) acc fields
    editors` _ acc = acc

sleep :: d Int -> Task () | toPrompt d
sleep d delta =
    get currentTimestamp >>= \(Timestamp t) ->
    wait d (\(Timestamp t`) -> t` >= t + delta) currentTimestamp @! ()

import Text.URI, TCPChannels, Internet.HTTP, Text

// do HTTP call bypassing the iTasks framework and measure response time
callHTTP` :: !HTTPMethod !URI !String !(HTTPResponse -> (MaybeErrorString a)) -> Task a | iTask a
callHTTP` method url=:{URI|uriScheme,uriRegName=Just uriRegName,uriPort,uriPath,uriQuery,uriFragment} data parseFun =
    worldIO callHTTP`` >>= \rsp ->
    case parseFun rsp of
        Ok r    -> return r
        Error e -> throw e
where
    callHTTP`` :: !*World -> *(!MaybeError String HTTPResponse, !*World)
    callHTTP`` w
        # (Just ip, w)                                   = lookupIPAddress uriRegName w
        # (_, Just {DuplexChannel|rChannel,sChannel}, w) = connectTCP_MT Nothing (ip, port) w
        # (sChannel,w)                                   = send (toByteSeq req) sChannel w
        # (data, rChannel, w)                            = getData [] rChannel w
        # w                                              = closeRChannel rChannel w
        # w                                              = closeChannel sChannel w
        = case parseResponse data of
            Nothing  -> (Error "Invalid response", w)
            Just rsp -> (Ok rsp, w)

    getData acc ch w
        # (_, mbData, ch, w) = receive_MT Nothing ch w
        = case mbData of
            Nothing   -> (concat (reverse acc), ch, w)
            Just data -> getData [toString data : acc] ch w

    port = fromMaybe 80 uriPort
    path = uriPath +++ maybe "" (\q -> ("?"+++q)) uriQuery +++ maybe "" (\f -> ("#"+++f)) uriFragment
    //VERY SIMPLE HTTP 1.1 Request
    req = toString method +++ " " +++ path +++ " HTTP/1.1\r\nHost:"+++uriRegName+++"\r\nConnection: close\r\n\r\n"+++data

getTimeMs :: Task Int
getTimeMs = worldIO getTimeMs`
where
    getTimeMs` :: *World -> *(!MaybeError Bool Int,!*World)
    getTimeMs` w 
        #(Clock c, w`) = clock w
        = (Ok (c / 1000), w`)

