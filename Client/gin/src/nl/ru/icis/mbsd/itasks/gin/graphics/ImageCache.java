package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.Image;
import java.awt.Toolkit;
import java.net.URL;
import java.util.HashMap;

import javax.swing.Icon;
import javax.swing.ImageIcon;

public class ImageCache {
	private static ImageCache instance = null;
	private HashMap<String, Image> cache;

	private ImageCache() {
		cache = new HashMap<String, Image>();
	}
	
	public static ImageCache getInstance() {
		if (instance == null)
			instance = new ImageCache();
		return instance;
	}
	
	public Image getImageFromName(String name) {
		Image result = cache.get(name);
		if (result == null) {
			String fileName = "images/" + name + ".png";
			URL url = ImageCache.class.getResource("/" + fileName);
			if (url != null)
				result = Toolkit.getDefaultToolkit().getImage(url);
			else //If running as application, load image from file
				result = Toolkit.getDefaultToolkit().getImage(fileName);
			cache.put(name, result);
			if (result == null)
				System.err.println("Image not found: " + fileName);
		}
		
		return result;
		
	}
	
	public Icon getIconFromName(String name) {
		Image image = getImageFromName(name);
		if (image == null)
			return null;
		else 
			return new ImageIcon(image);
	}
	
}
