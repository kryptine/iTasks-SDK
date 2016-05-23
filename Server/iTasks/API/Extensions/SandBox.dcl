definition module iTasks.API.Extensions.SandBox
import iTasks
from Data.Queue import :: Queue
from iTasks._Framework.TaskState import :: TIUIState
from iTasks.UI.Diff import :: UIUpdate, :: UIStep
from iTasks._Framework.Task import :: TaskResult, :: Event

derive class iTask TIUIState, Queue, Event, UIDef, UIUpdate, TaskRep, UIStep, UIContent, UIWindow
derive class iTask UIItemsOpts, UISizeOpts, UIWindowOpts, UIViewport, UIBlock, UIAction, UIForm, UIEmpty
derive class iTask UIControl, UIViewportOpts, UIActionOpts
derive class iTask UIViewOpts, UIHSizeOpts, UISize, UIBound, UISideSizes, UIFSizeOpts, UISliderOpts, UIProgressOpts
derive class iTask UIEditOpts, UIButtonOpts, UIChoiceOpts, UIGridOpts, UITreeNode, UITreeOpts, UIMenuButtonOpts, UILabelOpts, UIIconOpts
derive class iTask UIMenuItem, UITaskletOpts, UIEditletOpts, UIPanelOpts, UIFieldSetOpts, UITabSetOpts
derive class iTask UITab, UIEmbeddingOpts, UIWindowType, UIVAlign, UIHAlign, UITabOpts, UIDirection

/**
* Evaluate a task in an isolated context and control its I/O explicitly
*
*/
evalSandBoxed :: (Shared (Queue Event)) (Shared TIUIState) (Shared (TaskValue a)) (Task a) -> Task () | iTask a
