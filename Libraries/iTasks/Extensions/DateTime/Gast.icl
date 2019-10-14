implementation module iTasks.Extensions.DateTime.Gast

import StdEnv, Gast, iTasks.Extensions.DateTime

ggen{|Time|} _ = [{Time| hour = h, min = m, sec = s} \\ (h,m,s) <- diag3 [0..23] [0..59] [0..59]]
derive genShow Time
