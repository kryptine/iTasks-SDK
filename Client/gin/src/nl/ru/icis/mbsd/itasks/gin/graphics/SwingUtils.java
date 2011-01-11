package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.GridBagConstraints;

public class SwingUtils {
	public static GridBagConstraints makeGridBagConstraints(int x, int y, double weight,
			boolean hStretch) {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = x;
		gbc.gridy = y;
		gbc.weightx = weight;
		gbc.fill = hStretch ? GridBagConstraints.BOTH : GridBagConstraints.VERTICAL;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		return gbc;
	}

}
