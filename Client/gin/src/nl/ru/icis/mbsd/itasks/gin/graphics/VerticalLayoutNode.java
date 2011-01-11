package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.geom.Point2D;
import java.util.ListIterator;

import edu.umd.cs.piccolo.util.PBounds;

public class VerticalLayoutNode extends LayoutNode {

	private static final long serialVersionUID = 6020380989951256728L;

	public VerticalLayoutNode() {
		super();
		setPickable(false);
	}

	@SuppressWarnings("rawtypes")
	protected double getHorizontalReferenceLine(boolean invert) {
		double result = 0;
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			double reference = child.getReferencePoint().getX();
			if (invert)
				reference = 1.0 - reference;
			double childWidth = reference * child.getMinWidth();
			if (childWidth > result)
				result = childWidth;
		}
		return result;
	}

	@SuppressWarnings("rawtypes")
	@Override
	protected void layoutChildren() {
		double horizontalReference = getHorizontalReferenceLine(false);
		double rowSpace = getHorizontalReferenceLine(true);
		double top = 0;

		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			//Apply stretch
			if (child.isHorizontalStretch())
				child.setWidth(rowSpace);
			PBounds childBounds = child.getFullBoundsReference();
			Point2D.Double offset = new Point2D.Double();
			offset.x = horizontalReference
					- (child.getReferencePoint().getX() * childBounds
							.getWidth()) - child.getX();
			offset.y = top - child.getY();
			if (!child.getOffset().equals(offset))
				child.setOffset(offset);

			top += childBounds.getHeight();
		}
		super.layoutChildren();
	}

	@SuppressWarnings("rawtypes")
	@Override
	public boolean setHeight(double height) {
		int stretchCount = getVerticalStretchCount();
		if (stretchCount == 0)
			return false;
		double stretchNodeHeight = (height - getMinHeight()) / stretchCount;
		if (stretchNodeHeight < 0)
			stretchNodeHeight = 0;
		ListIterator i = getChildrenIterator();
		boolean changed = false;
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			if (child.isVerticalStretch()) {
				child.setHeight(stretchNodeHeight);
				changed = true;
			}
		}
		return changed;
	}

	@Override
	public boolean isVerticalStretch() {
		return getVerticalStretchCount() > 0;
	}
	
	@SuppressWarnings("rawtypes")
	private int getVerticalStretchCount() {
		int stretchCount = 0;
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			if (child.isVerticalStretch())
				stretchCount++;
		}
		return stretchCount;
	}
	
	@SuppressWarnings("rawtypes")
	@Override
	public double getMinHeight() {
		double minHeight = 0;
		ListIterator i = getChildrenIterator();
		while (i.hasNext()) {
			LayoutableNode child = (LayoutableNode) i.next();
			if (! child.isVerticalStretch())
				minHeight += child.getHeight();
		}
		return minHeight;
	}
}

