implementation module iTasks.Extensions.DateTime.Gast

import StdEnv, Gast, iTasks.Extensions.DateTime

ggen{|Time|} _ = [{Time| hour = h, min = m, sec = s} \\ (h,m,s) <- diag3 [0,23:[1..22]] [0,59:[1..58]] [0,59,60:[1..58]]]
derive genShow Time
