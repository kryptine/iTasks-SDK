implementation module iTasks.Extensions.Device.Camera
import iTasks
import iTasks.API.Extensions.Picture.JPEG
import iTasks.API.Extensions.Device._Common
from Text import class Text, instance Text String
import qualified Text as T

takePicture :: Task (Maybe JPEGPicture)
takePicture
	= catchAll (deviceRequest "takepicture" (\_ -> True)) (\_ -> return "")
	>>= \result -> unpack ('T'.split " " result)
where
	unpack :: [String] -> Task (Maybe JPEGPicture)
	unpack ["OK", image] 	= return (Just (JPEGPicture image))
	unpack _		= return Nothing
