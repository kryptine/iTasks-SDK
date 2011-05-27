implementation module InteractionTasks2

import InteractionTasks, StdMisc, StdString, StdClass

enter :: !(InputOptions a) -> Task a | iTask a
enter (IO d (Defined b a) acts IONoShare choice)
	= case acts of
		IONoActions				= if b (enterInformationAbout  d a)   (enterInformation      d)
		IOOfValue f				= if b (enterInformationAboutA d a f) (enterInformationAbout d a)
enter (IO d (Defined b a) acts share choice)
	= case share of
		IOShare        vf s		= if b (enterSharedInformationAbout  d vf a s)    (enterSharedInformation  d vf s)
		IOShareActions vf s af	= if b (enterSharedInformationAboutA d vf a s af) (enterSharedInformationA d vf s af)

:: InputOptions a
	= E.description about: 
	  IO description 
	     (Defined about)
	     (IOActions a)
	     (IOShare   a)
	     (IOChoice  a)
      & descr description 
      & iTask about

:: Defined a
	= Defined Bool a

none :: InputOptions a
none = IO "" 
          (Defined False 0)
          IONoActions
          IONoShare
          IONoChoice


instance <@ IODescr where
	<@ (IO _ a acts share choice) (IODescr d) = IO d a acts share choice
instance <@ IOAbout where
	<@ (IO d _ acts share choice) (IOAbout a) = IO d (Defined True a) acts share choice
instance <@ IOActions where
	<@ (IO d a _ share choice) acts = IO d a acts share choice
instance <@ IOShare where
	<@ (IO d a acts _ choice) share = IO d a acts share choice
instance <@ IOChoice where
	<@ (IO d a acts share _) choice = IO d a acts share choice

task = enter (none <@ (IODescr "Enter positive number")
                   <@ (IOAbout False)
                   <@ (IOOfValue actionsFun1)
             )
where
	actionsFun1 :: (Maybe String) -> [(Action,Maybe Int)]
	actionsFun1 Nothing  = [(ActionCancel,Just 0)]
	actionsFun1 (Just x) = [(ActionOk,if (x<>"\n") (Just 1) Nothing)]

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

class (<@) infixl 5 t :: !(InputOptions a) !(t a) -> InputOptions a
