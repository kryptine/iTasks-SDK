package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.geom.Point2D;
import edu.umd.cs.piccolo.util.PBounds;

public class MatrixLayoutNode extends LayoutNode {
	private static final long serialVersionUID = 8418722866372317692L;
	private int columns = 1;

	public MatrixLayoutNode() {
		super();
		setPickable(false);
	}

	public LayoutableNode getCell(int row, int column) {
		int index = row * columns + column;
		if (index >= getChildrenCount())
			return null;
		return (LayoutableNode) getChild(index);
	}

	public int getRowCount() {
		if (getColumnCount() == 0)
			return 0;
		else
			return (int) Math.ceil(getChildrenCount()
					/ (double) getColumnCount());
	}

	public int getColumnCount() {
		if (getChildrenCount() < columns)
			return getChildrenCount();
		else
			return columns;
	}

	public void setColumnCount(int value) {
		columns = value;
		layoutChildren();
	}

	protected double getHorizontalReferenceLine(int row, boolean invert) {
		double result = 0;
		for (int column = 0; column < getColumnCount(); column++) {
			LayoutableNode child = getCell(row, column);
			if (child == null)
				continue;
			double referenceY = child.getReferencePoint().getY();
			if (invert)
				referenceY = 1.0 - referenceY;
			double childHeight = referenceY * child.getMinHeight();
			if (childHeight > result)
				result = childHeight;
		}
		return result;
	}

	protected double getVerticalReferenceLine(int column, boolean invert) {
		double result = 0;
		for (int row = 0; row < getRowCount(); row++) {
			LayoutableNode child = getCell(row, column);
			if (child == null)
				continue;
			double referenceX = child.getReferencePoint().getX();
			if (invert)
				referenceX = 1.0 - referenceX;
			double childWidth = referenceX * child.getMinWidth();
			if (childWidth > result)
				result = childWidth;
		}
		return result;
	}

	protected static double stretchSize(double reference, double left,
			double right) {
		if (reference == 0)
			return right / (1.0 - reference);
		else if (reference == 1.0)
			return left / reference;
		else
			return Math.min(left / reference, right / (1.0 - reference));
	}

	@Override
	protected void layoutChildren() {
		double top = 0;
		for (int row = 0; row < getRowCount(); row++) {
			double horizontalReference = getHorizontalReferenceLine(row, false);
			double rowSpace = getHorizontalReferenceLine(row, true);
			double rowHeight = horizontalReference + rowSpace;
			double left = 0;
			for (int column = 0; column < getColumnCount(); column++) {
				double verticalReference = getVerticalReferenceLine(column,
						false);
				double columnSpace = getVerticalReferenceLine(column, true);
				double columnWidth = verticalReference + columnSpace;

				LayoutableNode child = getCell(row, column);
				if (child != null) {
					// Apply stretch
					if (child.isHorizontalStretch())
						child.setWidth(stretchSize(child
								.getReferencePoint().getX(), verticalReference,
								columnSpace));

					if (child.isVerticalStretch())
						child.setHeight(stretchSize(child
								.getReferencePoint().getY(),
								horizontalReference, rowSpace));

					// Calculate position
					PBounds childBounds = child.getFullBoundsReference();
					Point2D.Double offset = new Point2D.Double();
					offset.x = left
							+ verticalReference
							- (child.getReferencePoint().getX() * childBounds
									.getWidth()) - child.getX();
					offset.y = top
							+ horizontalReference
							- (child.getReferencePoint().getY() * childBounds
									.getHeight()) - child.getY();
					if (!child.getOffset().equals(offset))
						child.setOffset(offset);
				}
				left += columnWidth;
			}
			top += rowHeight;
		}
		super.layoutChildren();
	}
}
