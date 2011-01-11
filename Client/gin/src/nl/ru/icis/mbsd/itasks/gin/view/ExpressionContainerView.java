package nl.ru.icis.mbsd.itasks.gin.view;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.geom.Point2D;

import nl.ru.icis.mbsd.itasks.gin.DragAndDropTarget;
import nl.ru.icis.mbsd.itasks.gin.graphics.HorizontalLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.LayoutableNode;
import nl.ru.icis.mbsd.itasks.gin.model.Declaration;
import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.CleanExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.Expression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.ListComprehensionExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.ListExpression;
import nl.ru.icis.mbsd.itasks.gin.view.decorations.ErrorDecoration;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.ExpressionView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.GraphExpressionView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.ListComprehensionView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.ListExpressionView;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.TextExpressionView;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.nodes.PText;

public class ExpressionContainerView extends DecoratableView implements View, DragAndDropTarget {
	private static final long serialVersionUID = -4859817663098025502L;

	private ExpressionContainer expressionContainer;
	private PNode expressionView;
	
	public ExpressionContainerView(ExpressionContainer expressionContainer) {
		expressionContainer.addView(this);
		this.expressionContainer = expressionContainer;

		addDecoration(new ErrorDecoration(this, getExpressionContainer()));
		
		updateView(null);
	}
	
	public ExpressionContainer getExpressionContainer() {
		return expressionContainer;
	}
	
	private static ExpressionView createExpressionView(Expression expression) {
		if (expression instanceof GraphExpression)
			return new GraphExpressionView((GraphExpression) expression);
		else if (expression instanceof ListExpression)
			return new ListExpressionView((ListExpression) expression);
		else if (expression instanceof ListComprehensionExpression)
			return new ListComprehensionView((ListComprehensionExpression) expression);
		else if (expression instanceof CleanExpression)
			return new TextExpressionView((CleanExpression) expression);
		return null;
	}

	@Override
	public void updateView(Object source) {
		if (expressionView != null)
			removeChild(expressionView);
	
		Expression exp = expressionContainer.getExpression();
		if (exp != null) {
			expressionView = createExpressionView(exp);
			addChild(expressionView);
		}
		else {
			HorizontalLayoutNode hn = new HorizontalLayoutNode();
			for (Class <? extends Expression> expClass: getExpressionContainer().getSupportedExpressions()) {
				final Class <? extends Expression> expressionClass = expClass;
				PText text = new PText("[" + expressionClass.getSimpleName() + "]");
				text.setTextPaint(Color.blue);
				LayoutableNode textLayout = new LayoutableNode(text);
				textLayout.setReferencePoint(new Point2D.Double(0.0, 0.0));
				hn.addChild(textLayout);
				text.addInputEventListener(new PBasicInputEventHandler() {
					public void mouseClicked(final PInputEvent e) {
						try {
							ExpressionContainer exc = ExpressionContainerView.this.getExpressionContainer();
							exc.setExpression(expressionClass.newInstance());
							exc.setTypeExpressionContainer(exc.getTypeExpressionContainer());
							exc.notifyViews();
						} catch (InstantiationException e1) {
							e1.printStackTrace();
						} catch (IllegalAccessException e1) {
							e1.printStackTrace();
						}
						e.setHandled(true);
					}
					public void mouseEntered(PInputEvent e) {
						e.pushCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
					}
					public void mouseExited(PInputEvent e) {
						e.popCursor();
					}
				});
			}
			expressionView = hn;
			addChild(expressionView);
			
/*			JPanel panel = new JPanel();
			for (Class <? extends Expression> expClass: getExpressionContainer().getSupportedExpressions()) {
				final Class <? extends Expression> expressionClass = expClass;
				JButton button = new JButton(expressionClass.getSimpleName());
				panel.add(button);
				button.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						try {
							ExpressionContainer exc = ExpressionContainerView.this.getExpressionContainer();
							exc.setExpression(expressionClass.newInstance());
							exc.setType(exc.getType());
							exc.notifyViews();
						} catch (InstantiationException e1) {
							e1.printStackTrace();
						} catch (IllegalAccessException e1) {
							e1.printStackTrace();
						}
					}
				});
			}
			PFilterSwing swing = new PFilterSwing(panel);
			addChild(swing);*/
		}
	}

	@Override
	public boolean acceptDrag(Point2D position) {
		return expressionContainer.getExpression() == null;
	}

	@Override
	public void dragEnter(Point2D position) {
	}

	@Override
	public void dragExit() {
	}

	@Override
	public void drop(Declaration selection, Point2D position) {
		expressionContainer.setExpression(new GraphExpression());
		updateView(this);
		((GraphExpressionView)expressionView).drop(selection, position);
	}
}
