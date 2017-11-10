definition module iTasks.API.Extensions.Device.Camera

/**
 * This module provides tasks for device operation with the camera on an iTasks 
 * (node) server.
 */
 
import iTasks
import iTasks.API.Extensions.Picture.JPEG

/**
 * Take a picture with the device camera.
 */
takePicture :: Task (Maybe JPEGPicture)
