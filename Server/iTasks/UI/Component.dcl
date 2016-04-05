definition module iTasks.UI.Component

import iTasks.UI.Definition
import iTasks.UI.JS.Interface

:: Conflict :== Bool

:: ComponentDiff diff state = NoDiff | Diff diff 
                   (Conflict state *JSWorld -> *(state, ComponentDiff diff state, *JSWorld))
