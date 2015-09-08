definition module iTasks.API.Core.TaskLayout

from iTasks._Framework.Task import :: TaskId
from Graphics.Scalable.Types import :: XAlign, :: YAlign, :: GridDimension, :: GridLayout, :: GridXLayout, :: GridYLayout, :: GridMajor

:: TaskLayout = TaskLayout (TaskUITree -> TaskUILayout)

:: TaskUITree												// the abstract task structure on which to identify task UI elements
  = DefaultUI  !UITag										// the default rendering of the task identified by UITag
  | ParallelUI !UITag ![TaskUITree]							// the parallel sub-tasks of a parallel and its derived task combinators
  | StepUI     !UITag !TaskUITree ![(String,TaskUITree)]	// the step sub-task and its continuation-tasks
:: UITag (:== TaskId)

:: TaskUILayout
  = TaskUIDefault !UITag																	// use the task UI associated with UITag
  | TaskUICollage                                            ![(Int,Int)] ![TaskUILayout]	// exact placement in terms of (x-pixel,y-pixel) offsets
  | TaskUIBeside                          ![XAlign]          ![(Int,Int)] ![TaskUILayout]	// aligned and (x-pixel,y-pixel) offset beside each other (left-to-right)
  | TaskUIAbove                           ![YAlign]          ![(Int,Int)] ![TaskUILayout]	// aligned and (x-pixel,y-pixel) offset above  each other (top-to-bottom)
  | TaskUIGrid !GridDimension !GridLayout ![(XAlign,YAlign)] ![(Int,Int)] ![TaskUILayout]	// grid of given size, filled in given direction, 
																							// aligned and (x-pixel,y-pixel) offset
