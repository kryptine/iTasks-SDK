definition module iTasks.Extensions.DateTime.Gast

from iTasks.Extensions.DateTime import :: Date, :: Time, :: DateTime
from Gast                       import generic genShow, generic ggen, :: GenState

derive ggen    Date, Time, DateTime
derive genShow Date, Time, DateTime
