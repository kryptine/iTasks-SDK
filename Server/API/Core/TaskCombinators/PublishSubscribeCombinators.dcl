definition module PublishSubscribeCombinators

import StoreTasks, InteractionTasks

publishInformation :: question !(DBid a) -> Task (Action,a) | html question & iTask a 

subscribe :: message !(DBid a) -> Task a | html message & iTask a