module TestWindow

import iTasks

test = viewInformation "Press the button to open a window" [] ()
     >>| taskInWindow
     >>| viewInformation "Done" [] ()
where
    taskInWindow = (viewInformation (Title "Test window") [] "Hello!" >>* [OnAction ActionClose (always (return ()))]) <<@ InWindow

Start world = doTasks test world
