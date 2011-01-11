package nl.ru.icis.mbsd.itasks.gin.view.nodes;

import java.awt.geom.Point2D;

import nl.ru.icis.mbsd.itasks.gin.Toolbar;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;
import nl.ru.icis.mbsd.itasks.gin.view.DecoratableView;
import nl.ru.icis.mbsd.itasks.gin.view.Selectable;
import nl.ru.icis.mbsd.itasks.gin.view.View;
import nl.ru.icis.mbsd.itasks.gin.view.decorations.ErrorDecoration;
import nl.ru.icis.mbsd.itasks.gin.view.decorations.SelectionDecoration;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;

public abstract class NodeView extends DecoratableView implements View, Selectable {
	private static final long serialVersionUID = -4573489111918281225L;

	private Node node;
	private GraphExpressionView graphExpressionView;
	private boolean moved = false;

	public NodeView(final Node node,
			final GraphExpressionView graphExpressionView) {
		this.node = node;
		this.graphExpressionView = graphExpressionView;
		node.addView(this);
		node.getDeclaration().addDeepView(this);

		addDecoration(new SelectionDecoration(this, true));
		addDecoration(new ErrorDecoration(this, getNode()));
		
		addInputEventListener(new PBasicInputEventHandler() {
			public void mouseDragged(PInputEvent e) {
				super.mouseDragged(e);
				if (Toolbar.getInstance().getDrawingMode() == Toolbar.DrawingMode.ARROW) {
					// Drag: move a node
					double x = node.getPosition().getX()
							+ e.getCanvasDelta().getWidth();
					double y = node.getPosition().getY()
							+ e.getCanvasDelta().getHeight();

					if (x >= 0 && y >= 0) {
						node.setPosition(new Point2D.Double(node.getPosition()
								.getX()
								+ e.getCanvasDelta().getWidth(), node
								.getPosition().getY()
								+ e.getCanvasDelta().getHeight()));
						moved = true;
						updateView(null);
						graphExpressionView.updateEdges(NodeView.this);
					}
					e.setHandled(true);
				}
			}

			public void mouseReleased(PInputEvent e) {
				if (moved) {
					node.notifyViews();
					moved = false;
				}
			}
		});
	}

	public Node getNode() {
		return node;
	}

	public static NodeView Create(Node node,
			GraphExpressionView graphExpressionView) {
		NodeView result = null;
		String shapeArr[] = node.getDeclaration().getShape().split(":");
		if (shapeArr.length == 2) {
			String shape = shapeArr[0];
			String param = shapeArr[1];

			if (shape.equals("app"))
				result = new TaskApplicationNodeView(node, graphExpressionView);
			else if (shape.equals("icon"))
				result = new IconNodeView(node, graphExpressionView, param);
			else if (shape.equals("embed"))
				result = new EmbedNodeView(node, graphExpressionView, param);
			else if (shape.equals("comprehension"))
				result = new ComprehensionNodeView(node, graphExpressionView);
		}
		if (result == null) {
			System.err.println("Unknown shape:" + node.getDeclaration().getShape());
			result = new TaskApplicationNodeView(node, graphExpressionView);
		}
		
		return result;
	}

	@Override
	public void setSelected(boolean selected) {
		updateDecorations();
	}
	
	@Override
	public void Remove() {
		((GraphExpression) (graphExpressionView.getExpression()))
				.removeNode(node);
		graphExpressionView.getExpression().notifyViews();
	}

	@Override
	public void updateView(Object source) {
		super.updateView(source);
		node.checkRemovedParams();
		setOffset(node.getPosition());
	}
}
