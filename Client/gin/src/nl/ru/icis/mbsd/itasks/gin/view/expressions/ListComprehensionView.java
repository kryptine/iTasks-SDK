package nl.ru.icis.mbsd.itasks.gin.view.expressions;

import java.awt.geom.Point2D;

import nl.ru.icis.mbsd.itasks.gin.graphics.HorizontalLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.LayoutableNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.Rectangle;
import nl.ru.icis.mbsd.itasks.gin.graphics.VerticalLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.ListComprehensionExpression;
import nl.ru.icis.mbsd.itasks.gin.view.ExpressionContainerView;
import edu.umd.cs.piccolo.nodes.PText;

public class ListComprehensionView extends ExpressionView {
	private static final long serialVersionUID = -915016724165787796L;

	private Rectangle rect;

	public ListComprehensionView (ListComprehensionExpression listComprehensionExpression) {
		super (listComprehensionExpression);
		addDefaultDecorations();
		updateView(null);		
	}
	
	@Override
	public void updateView(Object source) {
		final ListComprehensionExpression expression = (ListComprehensionExpression) getExpression();

		if (rect != null)
			removeChild(rect);
		rect = new Rectangle(false);
		addChild(rect);
		VerticalLayoutNode vn = new VerticalLayoutNode();
		rect.addChild(vn);
		
		HorizontalLayoutNode row1 = new HorizontalLayoutNode();
		vn.addChild(row1);
		row1.setReferencePoint(new Point2D.Double(0.0, 0.0));
		row1.addChild(makeTextNode("for all "));
		row1.addChild(new LayoutableNode(new TextExpressionView(expression.getSelector())));
		row1.addChild(makeTextNode(" in "));
		row1.addChild(new LayoutableNode(new ExpressionContainerView(expression.getInput())));
		row1.addChild(makeTextNode(" given "));
		row1.addChild(new LayoutableNode(new TextExpressionView(expression.getGuard())));
		
		Rectangle outputRect = new Rectangle(false);
		LayoutableNode rowOutput = new LayoutableNode(outputRect);
		rowOutput.setReferencePoint(new Point2D.Double(0.0, 0.0));
		outputRect.addChild(new ExpressionContainerView(expression.getOutput()));
		vn.addChild(rowOutput);
	}
		
	private LayoutableNode makeTextNode(String s) {
		PText result = new PText();
		result.setFont(result.getFont().deriveFont((float) 12));
		result.setText(s);
		result.setPickable(false);
		return new LayoutableNode(result);
	}
}
