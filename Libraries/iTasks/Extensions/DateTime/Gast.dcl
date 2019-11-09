definition module iTasks.Extensions.DateTime.Gast

from iTasks.Extensions.DateTime import :: Time
from Gast                       import generic genShow, generic ggen, :: GenState

derive ggen    Time
derive genShow Time
