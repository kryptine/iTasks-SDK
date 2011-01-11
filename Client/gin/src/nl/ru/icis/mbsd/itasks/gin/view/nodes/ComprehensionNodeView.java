package nl.ru.icis.mbsd.itasks.gin.view.nodes;

import edu.umd.cs.piccolo.nodes.PText;

import nl.ru.icis.mbsd.itasks.gin.graphics.HorizontalLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.Rectangle;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.view.ExpressionContainerView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;

public class ComprehensionNodeView extends NodeView {
	private static final long serialVersionUID = 5143380017792928588L;
	private Rectangle rectangle;

	ComprehensionNodeView(Node node, GraphExpressionView graphExpressionView) {
		super(node, graphExpressionView);
		updateView(null);
	}
	
	@Override
	public void updateView(Object source) {
		if (rectangle != null)
			removeChild(rectangle);
		HorizontalLayoutNode hn = new HorizontalLayoutNode();
		hn.addChild(makeTextNode("{"));
		hn.addChild(makeExpression(2));
		hn.addChild(makeTextNode("|"));
		hn.addChild(makeExpression(0));
		hn.addChild(makeTextNode("<-"));
		hn.addChild(makeExpression(1));
		hn.addChild(makeTextNode("}"));
		
		rectangle = new Rectangle(false);
		rectangle.addChild(hn);
		addChild(rectangle);
		setBounds(getUnionOfChildrenBounds(null));
	}

	private PText makeTextNode(String s) {
		PText result = new PText();
		result.setFont(result.getFont().deriveFont((float) 32));
		result.setText(s);
		result.setPickable(false);
		return result;
	}

	private ExpressionContainerView makeExpression(int position) {
		return new ExpressionContainerView(getNode().getActualParam(getNode().getDeclaration().getFormalParameters().get(position)));
	}

}
