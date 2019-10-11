implementation module Incidone.Simulation.TrainingTasks
import iTasks, iTasks.Extensions.DateTime
import Text, System.FilePath
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
            Title "Simulate phonecall" @>> Hint "Type a phonenumber or leave blank for a call without caller identification" @>> enterInformation []
        >>* [OnAction (Action "Place call") (hasValue (\num -> reportPhoneCallBegin num Nothing))]
     ) @! ()


simulateEmail :: Task ()
simulateEmail
    = forever (
            Title "Simulate e-mail" @>> Hint "Type an e-mail message and press send to deliver it to Incidone directly" @>> enterInformation []
        >>* [OnAction (Action "Send") (hasValue injectEmail)]
    ) @! ()

simulateAIS :: Task ()
simulateAIS
    =   (Title "Simulate AIS" @>> Hint "You can simulate an AIS sensor feed" @>> manageBackgroundTask "ais-simulator" "AIS simulator" simulate)
    -|| (Hint "Last AIS import" @>> viewSharedInformation [] lastAISImport)
where
    simulate
       =   get applicationDirectory
       //>>- \dir -> forever (simulateStep (dir</> "Demo-content"</>"AIS"))

    simulateStep contentDir
       =   get currentTime //Fixme
/*
       >>- \now -> let updateTime = nextUpdate now in
           waitForTime updateTime
       >-| let fileName = (contentDir </> updatesFile updateTime) in
           importTextFile fileName
       >>- \content ->
           injectAISUpdates fileName (split "\n" content)
*/
    nextUpdate :: Time -> Time //Round time on 10 seconds
    nextUpdate time = time + {Time|hour=0,min=0,sec=10 - (time.Time.sec rem 10)}

    updatesFile :: Time -> FilePath
    updatesFile {Time|hour,min,sec} = addExtension (lpad (toString (hour rem 4)) 2 '0' +++ lpad (toString min) 2 '0' +++ lpad (toString sec) 2 '0') "txt"


