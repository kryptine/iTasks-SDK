implementation module Incidone.Simulation.TrainingTasks
import iTasks, Text, System.FilePath
import Incidone.OP.Concepts, Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.OP.CommunicationManagementTasks
import Incidone.Integration.AIS
import Incidone.Util.TaskPatterns

controlExercise :: Task ()
controlExercise
    = anyTask [simulatePhoneCalls,simulateEmail,simulateAIS]

simulatePhoneCalls :: Task ()
simulatePhoneCalls
    = forever (
            enterInformation ("Simulate phonecall","Type a phonenumber or leave blank for a call without caller identification") []
        >>* [OnAction (Action "Place call" []) (hasValue (\num -> reportPhoneCallBegin num Nothing))]
     ) @! ()


simulateEmail :: Task ()
simulateEmail
    = forever (
            enterInformation ("Simulate e-mail","Type an e-mail message and press send to deliver it to Incidone directly") []
        >>* [OnAction (Action "Send" []) (hasValue injectEmail)]
    ) @! ()

simulateAIS :: Task ()
simulateAIS
    =   manageBackgroundTask ("Simulate AIS","You can simulate an AIS sensor feed") "ais-simulator" "AIS simulator" simulate
    -|| viewSharedInformation "Last AIS import" [] lastAISImport
where
    simulate
       =   get applicationDirectory
       >>- \dir -> forever (simulateStep (dir</> "Demo-content"</>"AIS"))

    simulateStep contentDir
       =   get currentTime
       >>- \now -> let updateTime = nextUpdate now in
           waitForTime updateTime
       >>- \_ -> let fileName = (contentDir </> updatesFile updateTime) in
           importTextFile fileName
       >>- \content ->
           injectAISUpdates fileName (split "\n" content)

    nextUpdate :: Time -> Time //Round time on 10 seconds
    nextUpdate time = time + {Time|hour=0,min=0,sec=10 - (time.Time.sec rem 10)}

    updatesFile :: Time -> FilePath
    updatesFile {Time|hour,min,sec} = addExtension (lpad (toString (hour rem 4)) 2 '0' +++ lpad (toString min) 2 '0' +++ lpad (toString sec) 2 '0') "txt"


