package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.geom.Point2D;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.util.PBounds;

public class LayoutableNode extends PNode {
	private static final long serialVersionUID = -3724759523510260709L;
	private Point2D reference = new Point2D.Double(0.5, 0.5);
	private boolean horizontalStretch = false;
	private boolean verticalStretch = false;
	
	public LayoutableNode() {
	}
	
	public LayoutableNode(PNode child) {
		addChild(child);
		layoutChildren();
	}
					
	public Point2D getReferencePoint() {
		return reference;
	}

	public boolean isHorizontalStretch() {
		return horizontalStretch;
	}

	public boolean isVerticalStretch() {
		return verticalStretch;
	}

	public double getMinHeight() {
		return verticalStretch ? 0 : getHeight();
	}

	public double getMinWidth() {
		return horizontalStretch ? 0 : getWidth();
	}

	public void setHorizontalStretch(boolean b) {
		horizontalStretch = b;
	}

	public void setReferencePoint(Point2D point) {
		reference = point;
	}

	public void setVerticalStretch(boolean b) {
		verticalStretch = b;
	}
	
	@Override
    protected void layoutChildren() {
		//Update bounds
		if (getChildrenCount() > 0) {
			PBounds comparisonBounds = getUnionOfChildrenBounds(null);
			if (!comparisonBounds.equals(getBoundsReference())) {
				setBounds(comparisonBounds);
			}
		}
		else {
			setBounds(0, 0, 1, 1);
		}
    }	

	//For debugging
	public String getFullName() {
		StringBuilder sb = new StringBuilder();
		PNode node = this;
		while (node != null) {
			sb.insert(0, node.getClass().getSimpleName() + ".");
			node = (PNode) node.getParent();
		}
		
		return sb.toString();
	}
	
}
