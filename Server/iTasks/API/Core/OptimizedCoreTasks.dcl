definition module iTasks.API.Core.OptimizedCoreTasks
/**
* This module provides optimized specialized versions of the core 'basic tasks'
* this keeps the core set 'clean'. Derived tasks can use these for better performance when
* if not all expressive power is needed.
*/

import iTasks._Framework.Generic
import iTasks._Framework.SDS

from iTasks._Framework.Task	import :: Task
from iTasks.API.Core.Types import class descr, class Choice, :: Display

//Interact which yields the view value directly.
//This way it does not need to be encoded in the local l to access it
//The local value serves as 'hidden' state from which together with the view value the task value can be derived
interactExposed :: !d !(ReadOnlyShared r) (r -> (l,(v,InteractionMask))) (l r (v,InteractionMask) Bool Bool Bool -> (l,(v,InteractionMask))) (Maybe (Editor v))
                        -> Task (l,v) | descr d & iTask l & iTask r & iTask v

//This version does not use a share, and hence has a simpler update function and needs to store less state
interactLocalExposed :: !d (l,(v,InteractionMask)) (l (v,InteractionMask) Bool -> (l,(v,InteractionMask))) (Maybe (Editor v))
                        -> Task (l,v) | descr d & iTask l & iTask v

interactViewOnly :: !d !(ReadOnlyShared r) (r -> (v,InteractionMask)) (r (v,InteractionMask) Bool Bool Bool -> (v,InteractionMask)) (Maybe (Editor v))
                        -> Task v | descr d & iTask r & iTask v
interactLocalViewOnly :: !d (v,InteractionMask) ((v,InteractionMask) Bool -> (v,InteractionMask)) (Maybe (Editor v))
                        -> Task v | descr d & iTask v

interactNullEnter		:: !d !v (v->l) (Maybe (Editor v)) -> Task l | descr d & iTask v & iTask l
interactNullUpdate		:: !d !(l -> v) (l v -> l) (Maybe (Editor v)) l -> Task l | descr d & iTask l & iTask v
interactNullView 		:: !d (l->v) (Maybe (Editor v)) l -> Task l | descr d & iTask l & iTask v
interactSharedInformation	:: !d !(ReadOnlyShared r) (r -> v) (Maybe (Editor v)) -> Task r | descr d & iTask r & iTask v
