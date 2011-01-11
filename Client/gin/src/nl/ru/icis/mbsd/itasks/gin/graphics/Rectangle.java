package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.Color;
import java.awt.geom.RoundRectangle2D;

import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;

public class Rectangle extends PPath {

	private static final long serialVersionUID = 4056545211005763834L;

	public Rectangle(boolean round) {
		super();
		if (round) {
			RoundRectangle2D rr = new RoundRectangle2D.Double();
			rr.setRoundRect(0, 0, 50, 25, 5, 5);
			setPathTo(rr);
		} else {
			setPathToRectangle(0, 0, 1, 1);
		}
		setPaint(Color.white);
	}

	@Override
	protected void layoutChildren() {
		PBounds bounds = getUnionOfChildrenBounds(null);
		if (! bounds.equals(getBounds()))
			setBounds(getUnionOfChildrenBounds(null));
	}
}
