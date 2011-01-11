package nl.ru.icis.mbsd.itasks.gin.view;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;

import nl.ru.icis.mbsd.itasks.gin.model.Edge;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.TextExpressionView;
import nl.ru.icis.mbsd.itasks.gin.view.nodes.NodeView;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.nodes.PPath;

public class EdgeView extends PPath implements View, Selectable {
	private static final long serialVersionUID = 1345539604102333305L;

	private Edge edge;
	private GraphExpressionView graphExpressionView;
	private TextExpressionView patternView;
	private NodeView fromNode;
	private NodeView toNode;

	private PPath arrow;

	public EdgeView(final Edge edge,
			final GraphExpressionView graphExpressionView) {
		this.edge = edge;
		this.graphExpressionView = graphExpressionView;
		edge.addView(this);
		setFromNode(graphExpressionView
				.getNodeViewFromModel(edge.getFromNode()));
		setToNode(graphExpressionView.getNodeViewFromModel(edge.getToNode()));

		Point2D arrowPoints[] = { new Point(-1, 0), new Point(-6, -5),
				new Point(-6, 5)};
		arrow = new PPath();
		arrow.setPaint(Color.BLACK);
		arrow.setPathToPolyline(arrowPoints);
		addChild(arrow);

		addInputEventListener(new PBasicInputEventHandler() {
			public void mouseClicked(PInputEvent e) {
				super.mouseClicked(e);
				e.getInputManager().setKeyboardFocus(e.getPath());
				Selection.getInstance().set(EdgeView.this);
				e.setHandled(true);
			}
		});
		
		updateView(null);
	}
	
	public Edge getEdge() {
		return edge;
	}

	public NodeView getFromNode() {
		return fromNode;
	}

	public void setFromNode(NodeView fromNode) {
		this.fromNode = fromNode;
	}

	public NodeView getToNode() {
		return toNode;
	}

	public void setToNode(NodeView toNode) {
		this.toNode = toNode;
	}

	public void replaceNode(NodeView oldNode, NodeView newNode) {
		if (oldNode == fromNode)
			setFromNode(newNode);
		else if (oldNode == toNode)
			setToNode(newNode);
	}

	@Override
	public void setSelected(boolean selected) {
		setStroke(selected ? new BasicStroke(2.0f) : new BasicStroke());
		setStrokePaint(selected ? new Color(0, 0, 255) : Color.BLACK);
	}

	@Override
	public void updateView(Object source) {
		Point2D start = getEdgePoint(fromNode, toNode);
		Point2D end = getEdgePoint(toNode, fromNode);
		fromNode.localToGlobal(start);
		toNode.localToGlobal(end);
		globalToLocal(start);
		globalToLocal(end);
		reset();
		setPathTo(new Line2D.Double(start, end));

		arrow.setOffset(end);
		arrow.setRotation(Math.atan2(end.getY() - start.getY(), end.getX()
				- start.getX()));
		
		if (edge.getPattern() == null && patternView != null) {
			removeChild(patternView);
			patternView = null;
		}
		else if (edge.getPattern() != null && patternView == null) {
			patternView = new TextExpressionView(edge.getPattern());
			addChild(patternView);
		}

		if (patternView != null)
		{
			patternView.setOffset(getBoundsReference().getCenterX()
				- patternView.getWidth() / 2.0, getBoundsReference()
				.getCenterY()
				- patternView.getHeight() / 2.0);
		}
	}

	@Override
	public void Remove() {
		((GraphExpression) (graphExpressionView.getExpression()))
				.removeEdge(edge);
		graphExpressionView.getExpression().notifyViews();
	}

	public static Point2D getEdgePoint(PNode localNode, PNode remoteNode) {
		Point2D remotePoint = remoteNode.getBounds().getCenter2D();
		remoteNode.localToGlobal(remotePoint);
		localNode.globalToLocal(remotePoint);
		Point2D here = localNode.getBoundsReference().getCenter2D();
		double distanceX = Math.abs(here.getX() - remotePoint.getX());
		double distanceY = Math.abs(here.getY() - remotePoint.getY());

		if (distanceX > distanceY) {
			// Draw horizontal
			if (here.getX() > remotePoint.getX()) // left
				return new Point2D.Double(localNode.getBoundsReference()
						.getMinX(), localNode.getBoundsReference().getCenterY());
			else
				// right
				return new Point2D.Double(localNode.getBoundsReference()
						.getMaxX(), localNode.getBoundsReference().getCenterY());
		} else {
			// Draw vertical
			if (here.getY() > remotePoint.getY()) // left
				return new Point2D.Double(localNode.getBoundsReference()
						.getCenterX(), localNode.getBoundsReference().getMinY());
			else
				// right
				return new Point2D.Double(localNode.getBoundsReference()
						.getCenterX(), localNode.getBoundsReference().getMaxY());
		}
	}
}
