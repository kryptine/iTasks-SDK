package nl.ru.icis.mbsd.itasks.gin.view.nodes;

import edu.umd.cs.piccolo.nodes.PImage;
import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;

public class IconNodeView extends NodeView {
	private static final long serialVersionUID = 736725945299082425L;
	private String icon;
	
    IconNodeView(Node node, GraphExpressionView graphExpressionView, String icon) {
		super(node, graphExpressionView);
		this.icon = icon;
		updateView(null);
	}
	
	@Override
	public void updateView(Object source) {
		super.updateView(source);
		PImage image = new PImage(ImageCache.getInstance().getImageFromName(icon));
		image.setBounds(0, 0, 16, 16);
		addChild(image);
		setBounds(getUnionOfChildrenBounds(null));
	}
}
