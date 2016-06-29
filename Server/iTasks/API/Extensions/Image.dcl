definition module iTasks.API.Extensions.Image
/**
* This module provides support for displaying (interactive images)
*/
import iTasks

// Simple web images with just an URL
:: WebImage = WebImage !String

derive gText	        WebImage
derive gEditor			WebImage
derive JSONEncode		WebImage
derive JSONDecode		WebImage
derive gDefault			WebImage
derive gEq				WebImage
