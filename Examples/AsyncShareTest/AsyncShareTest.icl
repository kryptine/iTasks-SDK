module AsyncShareTest

import iTasks

derive class iTask ServerRole 

:: ServerRole = DomainServer | Client

testShare = sharedStore "sharedStoreName" 0

Start w = startEngine task w 

task = enterInformation "Enter server role" []
	>>= \role. (viewInformation "Current role" [] role 
		||- (case role of 
				DomainServer -> updateSharedInformation "" [] testShare
				Client -> viewSharedInformation "Bla" [] (remote testShare (DomainShare {DomainShareOptions| domain = "TEST"}))))




