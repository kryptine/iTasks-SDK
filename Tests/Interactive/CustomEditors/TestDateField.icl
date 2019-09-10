module TestDateField

import iTasks, iTasks.Util.Testing

test :: Task Date
test = testEditor gEditor{|*|} {Date|year=2003,mon=1,day=13} Update

Start world = doTasks test world
