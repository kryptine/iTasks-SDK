package nl.ru.icis.mbsd.itasks.gin.view.nodes;

import java.awt.Font;
import java.awt.Point;

import edu.umd.cs.piccolo.nodes.PImage;
import edu.umd.cs.piccolo.nodes.PText;

import nl.ru.icis.mbsd.itasks.gin.graphics.HorizontalLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.graphics.LayoutableNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.MatrixLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.Rectangle;
import nl.ru.icis.mbsd.itasks.gin.graphics.VerticalLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.FormalParameter;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.view.ExpressionContainerView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;

public class TaskApplicationNodeView extends NodeView {
	private static final long serialVersionUID = -765370913126160148L;
	
	private PImage iconImage ;
	private PText taskName;
	private MatrixLayoutNode paramsMatrix;

	private Rectangle rectangle;
	
	TaskApplicationNodeView(Node node, GraphExpressionView graphExpressionView) {
		super(node, graphExpressionView);
		rectangle = new Rectangle(true);
		addChild(rectangle);
		updateView(null);
	}
	
	public void updateView(Object source) {
		super.updateView(source);
		
		rectangle.removeAllChildren();
		HorizontalLayoutNode hn = new HorizontalLayoutNode();
		
		iconImage = new PImage();
		iconImage.setImage(ImageCache.getInstance().getImageFromName(getNode().getDeclaration().getIcon()));
		iconImage.setPickable(false);
		LayoutableNode imageLayout = new LayoutableNode(iconImage);
		imageLayout.setReferencePoint(new Point.Double(0.0, 0.0));
		iconImage.setBounds(0, 0, 16, 16);
		hn.addChild(imageLayout);
		
		VerticalLayoutNode vn  = new VerticalLayoutNode();
		vn.setReferencePoint(new Point.Double(0.0, 0.0));
		hn.addChild(vn);
		
		taskName = new PText();
		LayoutableNode taskLayout = new LayoutableNode(taskName); 
		taskLayout.setReferencePoint(new Point.Double(0.0, 0.0));
		taskName.setText(getNode().getDeclaration().getName());
		taskName.setFont(taskName.getFont().deriveFont(Font.BOLD));
		taskName.setPickable(false);
		vn.addChild(taskLayout);
		
		paramsMatrix = new MatrixLayoutNode();
		paramsMatrix.setReferencePoint(new Point.Double(0.0, 0.0));
		paramsMatrix.setColumnCount(2);
		vn.addChild(paramsMatrix);
		
		for (FormalParameter formalParameter : getNode().getDeclaration().getFormalParameters()) {
			//Parameter label
			PText paramName = new PText();
			LayoutableNode paramNameLayout = new LayoutableNode(paramName);
			paramNameLayout.setReferencePoint(new Point.Double(0.0, 0.5));
			paramName.setText(formalParameter.getName());
			paramName.setPickable(false);
			paramsMatrix.addChild(paramNameLayout);
			ExpressionContainer paramValue = getNode().getActualParam(formalParameter);
			ExpressionContainerView paramView = new ExpressionContainerView(paramValue);
			LayoutableNode paramViewLayout = new LayoutableNode(paramView);
			paramViewLayout.setReferencePoint(new Point.Double(0.0, 0.5));
			paramsMatrix.addChild(paramViewLayout);
		}
		rectangle.addChild(hn);
	}
}
