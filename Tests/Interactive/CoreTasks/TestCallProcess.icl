module TestCallProcess
import iTasks

test = Hint "Press the button to run an OS process" @>> viewInformation [] ()
     >!| withShared []
        \io -> (externalProcess "/bin/date" [] Nothing io {onStartup=onStartup
                                                          ,onOutData=onOutData
                                                          ,onErrData=onErrData
                                                          ,onShareChange=onShareChange
                                                          ,onExit=onExit} Nothing gEditor{|*|}
                    -|| (Hint "OUTPUT: " @>> viewSharedInformation  [] io)
                    )
where
    onStartup r = (Ok r, Nothing, [], False)

    onOutData data l r  = (Ok [data:l], Just [data:r], [], False)
    onErrData _ l r = (Ok l, Nothing, [], False)
    onShareChange l r = (Ok l, Nothing, [], False)

    onExit c l r = (Ok l, Nothing)

Start world = doTasks test world
