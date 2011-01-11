package nl.ru.icis.mbsd.itasks.gin.view.expressions;

import java.awt.Cursor;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.util.ListIterator;

import nl.ru.icis.mbsd.itasks.gin.DragAndDropTarget;
import nl.ru.icis.mbsd.itasks.gin.Toolbar;
import nl.ru.icis.mbsd.itasks.gin.graphics.PathUtils;
import nl.ru.icis.mbsd.itasks.gin.model.Declaration;
import nl.ru.icis.mbsd.itasks.gin.model.Edge;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;
import nl.ru.icis.mbsd.itasks.gin.view.EdgeView;
import nl.ru.icis.mbsd.itasks.gin.view.Selectable;
import nl.ru.icis.mbsd.itasks.gin.view.Selection;
import nl.ru.icis.mbsd.itasks.gin.view.decorations.SelectionDecoration;
import nl.ru.icis.mbsd.itasks.gin.view.nodes.NodeView;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;

public class GraphExpressionView extends ExpressionView implements
		DragAndDropTarget {
	private static final long serialVersionUID = 3155641437435166973L;

	private PNode nodes;
	private PNode edges;
	private PPath newEdgePath = null;
	private Point2D mousePressPosition = null;

	private PPath rectangle;
	private boolean resized = false;
	
	private static final int CANVAS_HEIGHT = 4000;
	private static final int CANVAS_WIDTH = 4000;

	public GraphExpressionView(GraphExpression graphExpression) {
		super(graphExpression);
		if (graphExpression.hasSize())
			addDefaultDecorations();
		else
			addDecoration(new SelectionDecoration(this, false));
		
		graphExpression.addView(this);
		
		nodes = new PNode();
		nodes.setPickable(false);
		addChild(nodes);
		edges = new PNode();
		edges.setPickable(false);
		addChild(edges);
		nodes.moveInFrontOf(edges);
		
		if (graphExpression.hasSize()) {
			rectangle = new PPath();
			rectangle.setPickable(true);
			rectangle.setPathToRectangle(0, 0, 1, 1);
			rectangle.addInputEventListener(new PBasicInputEventHandler() {
				public void mouseEntered(PInputEvent e) {
					Point2D pos = e.getPositionRelativeTo(rectangle);
					if (pos.getX() >= rectangle.getWidth() / 2
							|| pos.getY() >= rectangle.getHeight() / 2)
						e.pushCursor(Cursor
								.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR));
				}

				public void mouseExited(PInputEvent e) {
					Point2D pos = e.getPositionRelativeTo(rectangle);
					if (pos.getX() >= rectangle.getWidth() / 2
							|| pos.getY() >= rectangle.getHeight() / 2)
						e.popCursor();
				}

				public void mouseDragged(PInputEvent e) {
					if (e.isLeftMouseButton()) {
						Point2D pos = e.getPositionRelativeTo(rectangle);
						pos = new Point2D.Double(Math.max(20, pos.getX()), Math.max(15, pos.getY()));
						System.out.println(pos);
						if (pos.getX() >= rectangle.getWidth() / 2
								|| pos.getY() >= rectangle.getHeight() / 2) {
							GraphExpression exp = ((GraphExpression) getExpression());
							exp.setWidth(pos.getX());
							exp.setHeight(pos.getY());
//							exp.setWidth(exp.getWidth()
//									+ e.getCanvasDelta().getWidth());
//							exp.setHeight(exp.getHeight()
//									+ e.getCanvasDelta().getHeight());
							resized = true;
							updateSize();
							e.setHandled(true);
						}
					}
				}

				public void mouseReleased(PInputEvent e) {
					if (resized) {
						((GraphExpression) getExpression()).notifyViews();
						resized = false;
					}
				}
			});
			addChild(rectangle);
		}

		addInputEventListener(new PBasicInputEventHandler() {
			public void mousePressed(PInputEvent e) {
				if (Toolbar.getInstance().getDrawingMode() == Toolbar.DrawingMode.CONNECTOR) {
					newEdgePath = new PPath();
					addChild(newEdgePath);
					mousePressPosition = e.getPosition();
					newEdgePath.globalToLocal(mousePressPosition);
					e.setHandled(true);
				}
			}

			public void mouseReleased(PInputEvent e) {
				if (newEdgePath != null) {
					addEdgeFromPositions(mousePressPosition, e.getPosition());
					removeChild(newEdgePath);
					newEdgePath = null;
					e.setHandled(true);
				}
			}

			public void mouseDragged(PInputEvent e) {
				super.mouseDragged(e);
				if (newEdgePath != null) {
					// Drag: create line
					Point2D currentPosition = e.getPosition();
					newEdgePath.globalToLocal(currentPosition);
					newEdgePath.setPathTo(new Line2D.Double(
							mousePressPosition, currentPosition));
					e.setHandled(true);
				}
			}

			// Delete selected node if backspace or delete pressed
			public void keyTyped(PInputEvent e) {
				super.keyTyped(e);
				Selectable sel = Selection.getInstance().get();
				if ((e.getKeyChar() == 8 || e.getKeyChar() == 127) && sel != null && sel instanceof NodeView) {
					Selection.getInstance().removeSelection();
					getExpression().notifyViews();
				}
			}
		});
		updateView(null);
	}

	public void updateView(Object source) {
		GraphExpression graphExpression = (GraphExpression) getExpression();

		nodes.removeAllChildren();
		for (Node modelNode : graphExpression.getNodes()) {
			NodeView nodeView = NodeView.Create(modelNode, this);
			nodeView.updateView(null);
			nodes.addChild(nodeView);
		}
		
		edges.removeAllChildren();
		for (Edge modelEdge : graphExpression.getEdges()) {
			EdgeView edgeView = new EdgeView(modelEdge, this);
			edges.addChild(edgeView);
		}		
//		updateEdges();
//		updateSize();
	}

	public void updateEdges(Object source) {
		@SuppressWarnings("rawtypes")
		ListIterator iterator = edges.getChildrenIterator();
		while (iterator.hasNext())
			((EdgeView) iterator.next()).updateView(source);
	}
	
	private void updateSize() {
		GraphExpression graphExpression = (GraphExpression) getExpression();
		if (rectangle != null) {
			PBounds nodeBounds = nodes.getUnionOfChildrenBounds(null);
			double width = Math.max(graphExpression.getWidth(), nodeBounds.x + nodeBounds.width);
			double height = Math.max(graphExpression.getHeight(), nodeBounds.y + nodeBounds.height);
			rectangle.setBounds(0, 0, width, height);
			setBounds(rectangle.getBounds());
		}
		else {
			setBounds(0, 0,  CANVAS_WIDTH, CANVAS_HEIGHT);
		}
	}
	
	public NodeView getNodeViewFromModel(Node node) {
		@SuppressWarnings("rawtypes")
		ListIterator iterator = nodes.getChildrenIterator();
		while (iterator.hasNext()) {
			NodeView nodeView = (NodeView) iterator.next();
			if (nodeView.getNode() == node)
				return nodeView;
		}
		return null;
	}

	public NodeView getNodeViewFromPosition(Point2D position) {
		@SuppressWarnings("rawtypes")
		ListIterator iterator = nodes.getChildrenIterator();
		while (iterator.hasNext()) {
			NodeView nodeView = (NodeView) iterator.next();
			PBounds bounds = nodeView.getBounds();
			nodeView.localToGlobal(bounds);
			globalToLocal(bounds);
			if (PathUtils.intersects(position, bounds))
				return nodeView;
		}
		return null;
	}

	public void addEdgeFromPositions(Point2D fromPosition, Point2D toPosition) {
		NodeView fromNodeView = getNodeViewFromPosition(globalToLocal(fromPosition));
		NodeView toNodeView = getNodeViewFromPosition(globalToLocal(toPosition));
		if (fromNodeView == null || toNodeView == null
				|| fromNodeView == toNodeView)
			return;

		Edge edge = new Edge();
		edge.setFromNode(fromNodeView.getNode());
		edge.setToNode(toNodeView.getNode());
		((GraphExpression) getExpression()).addEdge(edge);
		((GraphExpression) getExpression()).notifyViews();
	}

	@Override
	public boolean acceptDrag(Point2D position) {
		return true;
	}

	@Override
	public void dragEnter(Point2D position) {
	}

	@Override
	public void dragExit() {
	}

	@Override
	public void drop(Declaration declaration, Point2D position) {
		position = globalToLocal(position);
		GraphExpression graphExpression = (GraphExpression) getExpression();
		Node node = new Node(declaration);
		node.setPosition(position);
		graphExpression.addNode(node);
		((GraphExpression) getExpression()).notifyViews();
	}
	
	@Override
	protected void layoutChildren() {
		super.layoutChildren();
		updateEdges(null);
		updateSize();
	}
}
