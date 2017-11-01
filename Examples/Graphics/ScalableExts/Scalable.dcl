definition module ScalableExts.Scalable

/**	This module extends Graphics.Scalable.
*/
import Graphics.Scalable

/**	circular r a imgs = image:
	displays @imgs along an arc of radius @r, starting at angle @a.
*/
circular :: !Span !Real ![Image m] -> Image m
