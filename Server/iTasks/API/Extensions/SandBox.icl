implementation module iTasks.API.Extensions.SandBox
import iTasks
import iTasks._Framework.Task
import iTasks._Framework.TaskState
import iTasks.UI.Diff, iTasks.UI.Definition
import Data.Queue

derive class iTask TIUIState, Queue, Event, UIDef, UIUpdate, TaskRep, UIStep, UIContent, UIWindow
derive class iTask UIItemsOpts, UISizeOpts, UIWindowOpts, UIViewport, UIBlock, UIAction, UIForm, UIEmpty
derive class iTask UIControl, UIViewportOpts, UIActionOpts
derive class iTask UIViewOpts, UIHSizeOpts, UISize, UIBound, UISideSizes, UIFSizeOpts, UISliderOpts, UIProgressOpts
derive class iTask UIEditOpts, UIButtonOpts, UIChoiceOpts, UIGridOpts, UITreeNode, UITreeOpts, UIMenuButtonOpts, UILabelOpts, UIIconOpts
derive class iTask UIMenuItem, UITaskletOpts, UIEditletOpts, UIPanelOpts, UIFieldSetOpts, UITabSetOpts
derive class iTask UITab, UIEmbeddingOpts, UIWindowType, UIVAlign, UIHAlign, UITabOpts, UIDirection

evalSandBoxed :: (Shared (Queue Event)) (Shared TIUIState) (Shared (TaskValue a)) (Task a) -> Task () | iTask a
evalSandBoxed input output result task = return ()
