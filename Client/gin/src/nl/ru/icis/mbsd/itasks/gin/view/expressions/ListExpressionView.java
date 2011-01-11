package nl.ru.icis.mbsd.itasks.gin.view.expressions;

import java.awt.geom.Point2D;

import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.graphics.LayoutableNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.MatrixLayoutNode;
import nl.ru.icis.mbsd.itasks.gin.graphics.Rectangle;
import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.ListExpression;
import nl.ru.icis.mbsd.itasks.gin.view.ExpressionContainerView;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.nodes.PImage;
import edu.umd.cs.piccolo.nodes.PText;

public class ListExpressionView extends ExpressionView {
	private static final long serialVersionUID = 8728191361623931537L;
	private Rectangle rect;

	public ListExpressionView (ListExpression listExpression) {
		super (listExpression);
		addDefaultDecorations();
		updateView(null);		
	}
	
	@Override
	public void updateView(Object source) {
		final ListExpression listExpression = (ListExpression) getExpression();

		if (rect != null)
			removeChild(rect);
		
		MatrixLayoutNode grid = new MatrixLayoutNode();
		rect = new Rectangle(false);
		rect.addChild(grid);
		addChild(rect);
		grid.setColumnCount(3);
		
		for (int i = 0; i < listExpression.getItems().size(); i++) {
			ExpressionContainer item = listExpression.getItems().get(i);
			
			final int index = i;
			LayoutableNode itemView = new LayoutableNode(new ExpressionContainerView(item));
			itemView.setReferencePoint(new Point2D.Double(0.0, 0.5));
			grid.addChild(itemView);
			
			PImage addButton = new PImage(ImageCache.getInstance().getImageFromName("add16"));
			grid.addChild(new LayoutableNode(addButton));
			addButton.addInputEventListener(new PBasicInputEventHandler() {
				@Override
				public void mouseClicked(PInputEvent e) {
					listExpression.addExpressionContainer(index);
					listExpression.notifyViews();				
					e.setHandled(true);
				}
			});
			
			PImage removeButton = new PImage(ImageCache.getInstance().getImageFromName("remove16"));
			grid.addChild(new LayoutableNode(removeButton));
			removeButton.addInputEventListener(new PBasicInputEventHandler() {
				@Override
				public void mouseClicked(PInputEvent e) {
					listExpression.removeExpression(listExpression.getItems().get(index));
					listExpression.notifyViews();
					e.setHandled(true);
				}
			});
		}
		if (listExpression.getItems().isEmpty()) {
			grid.addChild(new LayoutableNode(new PText("(empty list)")));
			PImage addButton = new PImage(ImageCache.getInstance().getImageFromName("add16"));
			LayoutableNode addLayout = new LayoutableNode(addButton);
			addLayout.setReferencePoint(new Point2D.Double(0.0, 0.5));
			grid.addChild(addLayout);
			addButton.addInputEventListener(new PBasicInputEventHandler() {
				@Override
				public void mouseClicked(PInputEvent e) {
					listExpression.addExpressionContainer(0);
					listExpression.notifyViews();				
					e.setHandled(true);
				}
			});
			grid.invalidateLayout();
		}
	}
}
