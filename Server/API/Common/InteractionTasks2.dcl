definition module InteractionTasks2

from StdFunc import id, const
import CoreTasks, InteractionTasks

enter :: !(InputOptions a) -> Task a | iTask a

:: InputOptions a

class (<@) infixl 5 t :: !(InputOptions a) !(t a) -> InputOptions a
instance <@ IODescr
instance <@ IOAbout
instance <@ IOActions
instance <@ IOShare
instance <@ IOChoice

:: IODescr   a = E.d:   IODescr d & descr d
:: IOAbout   a = E.d:   IOAbout d & iTask d
:: IOActions a =        IONoActions
             | E.v:     IOOfValue (ActionFunc (Maybe v) a) & iTask v
:: IOShare   a =        IONoShare
             | E.v   w: IOShare (v a -> w) (Shared a w) & iTask v & iTask w
             | E.v r w: IOShareActions (v r -> w) (Shared r w) (ActionFunc (InformationState r) a) & iTask r & iTask v & iTask w
:: IOChoice  a =        IONoChoice
             | E.o v:   IOChoice (o -> v) [o] & iTask o & iTask v
             | E.o v:   IOChoiceActions (o -> v) [o] (ActionFunc (Maybe o) a) & iTask o & iTask v
