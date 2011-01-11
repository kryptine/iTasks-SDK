package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.geom.Point2D;
import java.util.ListIterator;

import edu.umd.cs.piccolo.util.PBounds;

public class HorizontalLayoutNode extends LayoutNode {
	
	private static final long serialVersionUID = -7852014462029880205L;
	
	public HorizontalLayoutNode()
	{
		super();
		setPickable(false);
	}
	
	@SuppressWarnings("rawtypes")
	protected double getHorizontalReferenceLine(boolean invert) {
		double result = 0;
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			double reference = child.getReferencePoint().getY();
			if (invert)
				reference = 1.0 - reference;
			double childHeight = reference * child.getMinHeight();
			if (childHeight > result) 
				result = childHeight;
		}
		return result;
	}
		
	@SuppressWarnings("rawtypes")
	@Override
	protected void layoutChildren() {
		double getReferenceLine = getHorizontalReferenceLine(false);
		double rowSpace = getHorizontalReferenceLine(true);

		double left = 0;
		
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			//Apply stretch
			if (child.isHorizontalStretch())
				child.setHeight(rowSpace);
			PBounds childBounds = child.getFullBoundsReference();
			Point2D.Double offset = new Point2D.Double();
			offset.x = left - child.getX();
			offset.y = getReferenceLine - (child.getReferencePoint().getY() * childBounds.getHeight()) - child.getY();
			if (! child.getOffset().equals(offset))
				child.setOffset(offset);
			left += childBounds.getWidth();
		}
		super.layoutChildren();
	}
	
	@SuppressWarnings("rawtypes")
	@Override
	public boolean setWidth(double width) {
		int stretchCount = getHorizontalStretchCount();
		if (stretchCount == 0)
			return false;
		double stretchNodeWidth = (width - getMinHeight()) / stretchCount;
		if (stretchNodeWidth < 0)
			stretchNodeWidth = 0;
		ListIterator i = getChildrenIterator();
		boolean changed = false;
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			if (child.isHorizontalStretch()) {
				child.setWidth(stretchNodeWidth);
				changed = true;
			}
		}
		return changed;
	}

	@Override
	public boolean isHorizontalStretch() {
		return getHorizontalStretchCount() > 0;
	}
	
	@SuppressWarnings("rawtypes")
	private int getHorizontalStretchCount() {
		int stretchCount = 0;
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			if (child.isHorizontalStretch())
				stretchCount++;
		}
		return stretchCount;
	}
	
	@SuppressWarnings("rawtypes")
	@Override
	public double getMinWidth() {
		double minHeight = 0;
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			if (! child.isHorizontalStretch())
				minHeight += child.getWidth();
		}
		return minHeight;
	}
}
