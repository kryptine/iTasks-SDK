implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
/*
import iTasks.UI.Layout.Common
import iTasks.UI.Definition
import Text.GenJSON
import Data.GenEq

from Data.Func import $
from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor, Data.Maybe
import Data.List, StdString
import qualified Data.Map as DM
*/
import iTasks.UI.Layout.Minimal
import iTasks.UI.Layout.BasicForms
import iTasks.UI.Layout.StandardForms

defaultSessionLayout :: LayoutRule
defaultSessionLayout = standardFormsSessionLayout
