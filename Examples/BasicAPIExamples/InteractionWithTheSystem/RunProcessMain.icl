module BasicAPIExamples.InteractionWithTheSystem.RunProcessMain
import BasicAPIExamples.InteractionWithTheSystem.RunProcess

Start :: *World -> *World
Start w = doTasks main w
