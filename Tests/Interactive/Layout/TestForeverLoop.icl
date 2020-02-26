module TestForeverLoop

test = forever (
        viewInformation [] "From one screen..."
    >!| viewInformation [] "To the next..."
    >!| return ()
    )

Start world = doTasks test world
