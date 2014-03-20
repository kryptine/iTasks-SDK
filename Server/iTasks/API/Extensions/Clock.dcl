definition module iTasks.API.Extensions.Clock
/**
* This module provides a type for visualizing time as an an analog clock
*/
import iTasks

:: AnalogClock = AnalogClock !Time

derive JSONEncode AnalogClock
derive JSONDecode AnalogClock
derive gEditor AnalogClock
derive gEditMeta AnalogClock
derive gUpdate AnalogClock
derive gVerify AnalogClock
derive gEq AnalogClock
derive gDefault AnalogClock
derive gVisualizeText AnalogClock

