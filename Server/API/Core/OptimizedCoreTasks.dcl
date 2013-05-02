definition module OptimizedCoreTasks
/**
* This module provides optimized specialized versions of the core 'basic tasks'
* this keeps the core set 'clean'. Derived tasks can use these for better performance when
* if not all expressive power is needed.
*/

import iTasks.Framework.iTaskClass, iTasks.Framework.Shared
from iTasks.Framework.Task	import :: Task

interactNullEnter		:: !d !v (v->l) -> Task l | descr d & iTask v & iTask l
interactNullUpdate		:: !d !(l -> v) (l v -> l) l -> Task l | descr d & iTask l & iTask v
interactNullView		:: !d (l->v) l -> Task l | descr d & iTask l & iTask v
interactSharedChoice	:: !d !(ReadOnlyShared r) (Maybe l) (r (Maybe l) -> t v l)
							-> Task (Maybe l) | descr d & Choice t & iTask r & iTask l & iTask (t v l)

interactSharedChoiceNoView	:: !d !(ReadOnlyShared r) (Maybe l) (r (Maybe l) -> t l)
								-> Task (Maybe l) | descr d & ChoiceNoView t & iTask r & iTask l & iTask (t l)
interactSharedInformation	:: !d !(ReadOnlyShared r) (r -> v) -> Task r | descr d & iTask r & iTask v
