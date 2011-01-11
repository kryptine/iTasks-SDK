package nl.ru.icis.mbsd.itasks.gin.view.nodes;

import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.FormalParameter;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.view.ExpressionContainerView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;

public class EmbedNodeView extends NodeView {
	private static final long serialVersionUID = -3440521722243601929L;
	private String shape;
	private ExpressionContainerView paramView;
	private PPath pathNode;

	EmbedNodeView(Node node, GraphExpressionView graphExpressionView,
			String shape) {
		super(node, graphExpressionView);
		this.shape = shape;
		updateView(null);
	}
		
	@Override
	public void updateView(Object source) {
		super.updateView(source);
		
		if (pathNode != null)
			removeChild(pathNode);

		pathNode = new PPath();
		if (shape.equals("rhombus")) {
			pathNode.reset();
			pathNode.moveTo(1, 0);
			pathNode.lineTo(2, 1);
			pathNode.lineTo(1, 2);
			pathNode.lineTo(0, 1);
			pathNode.lineTo(1, 0);
		}
		else if (shape.equals("ellipse")) {
			pathNode.setPathToEllipse(0, 0, 2, 2);
		}
		
		FormalParameter formalParameter = getNode().getDeclaration()
		.getFormalParameters().get(0);
		ExpressionContainer paramValue = getNode().getActualParam(formalParameter);
		paramView = new ExpressionContainerView(paramValue);
		pathNode.addChild(paramView);
		
		addChild(pathNode);
		layoutChildren();
	}

	@Override
	protected void layoutChildren() {
		PBounds paramBounds = paramView.getBounds();
		paramView.setOffset(0.5 * paramBounds.getWidth(), 0.5 * paramBounds.getHeight());
		pathNode.setBounds(0, 0, 2 * paramBounds.getWidth(), 2 * paramBounds.getHeight());
		setBounds(pathNode.getBounds());
		super.layoutChildren();
	}
}
