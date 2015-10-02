definition module iTasks.API.Core.TaskLayout

import StdOverloaded
from iTasks._Framework.Task import :: TaskId
from iTasks.UI.Definition import :: UIDef
from iTasks.API.Core.TaskCombinators import class tune
from Graphics.Scalable import :: XAlign(..), :: YAlign(..), :: GridDimension(..), :: GridLayout(..), :: GridXLayout(..), :: GridYLayout(..), :: GridMajor(..)
from iTasks._Framework.Generic.Visualization import generic gText, :: TextFormat
from Text.JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
import Data.Maybe

:: TaskLayout = TaskLayout (TaskUITree -> TaskUILayout)

:: TaskUITree
  = UI    !UITag					// the default rendering of the task identified by UITag
  | Par   !UITag ![TaskUITree]		// the parallel sub-tasks of a parallel and its derived task combinators
  | LStep !UITag ! TaskUITree		// the step left sub-task
  | RStep !UITag ! TaskUITree		// the step chosen right sub-task
:: UITag

:: TaskUILayout

uiOf      :: !TaskUITree -> TaskUILayout
uiCollage ::                                               ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiBeside  ::                            ![YAlign]          ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiAbove   ::                            ![XAlign]          ![(Int,Int)] ![TaskUILayout] -> TaskUILayout
uiGrid    :: !GridDimension !GridLayout ![(XAlign,YAlign)] ![(Int,Int)] ![TaskUILayout] -> TaskUILayout

derive JSONEncode TaskLayout, UITag, TaskUILayout, TaskUITree, XAlign, YAlign, GridXLayout, GridYLayout, GridMajor, GridDimension
derive JSONDecode TaskLayout, UITag, TaskUILayout, TaskUITree, XAlign, YAlign, GridXLayout, GridYLayout, GridMajor, GridDimension
derive gText      TaskLayout, UITag, TaskUILayout, TaskUITree, XAlign, YAlign, GridXLayout, GridYLayout, GridMajor, GridDimension

instance == UITag
instance  < UITag
instance toString UITag
mkUITag   :: !TaskId -> UITag
unmkUITag :: !UITag -> TaskId

uitag :: !TaskUITree -> UITag

instance tune TaskLayout
layout_tags ::                         !TaskUILayout -> [UITag]
layoutTasks :: ![(TaskId,Maybe UIDef)] !TaskUILayout -> Maybe UIDef
