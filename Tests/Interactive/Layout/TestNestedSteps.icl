module TestNestedSteps
import iTasks

test =   viewInformation [] "Step 1"
     >>| viewInformation [] "Step 2"
     >>| viewInformation [] "Step 3"
     >>| viewInformation [] "Step 4"

Start world = doTasks test world
