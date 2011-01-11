package nl.ru.icis.mbsd.itasks.gin.graphics;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import edu.umd.cs.piccolo.nodes.PPath;

public class PathUtils {

	public enum CornerDirection {
		NORTHWEST, NORTHEAST, SOUTHWEST, SOUTHEAST
	};
	
	public static PPath makeDummy () {
		PPath dummy = new PPath();
		dummy = PPath.createEllipse(0, 0, 10, 10);
		return dummy;
	}
	
	public static PPath makeEmpty() {
		return makeSpacer(1, 1);
	}
	
	public static PPath makeHSpacer(double width) {
		return makeSpacer(width, 1);
	}
	public static PPath makeVSpacer(double height) {
		return makeSpacer(1, height);
	}
	
	public static PPath makeSpacer(double width, double height) {
		PPath empty = new PPath();
		empty.setBounds(0, 0, width, height);
		return empty;
	}

	public static PPath makeArrow() {
		Point2D arrowPoints[] = { new Point(5, 0), new Point(5, 25),
				new Point(0, 20), new Point(5, 25), new Point(10, 20) };
		PPath arrow = new PPath();
		arrow.setPathToPolyline(arrowPoints);
		return arrow;
	}
/*
	public static PPath makeHLineStretch(double horizontalReference) {
		Point2D hlinePoints[] = { new Point(0, 0), new Point(1, 0) };
		PPath line = new PPath();
		line.setPathToPolyline(hlinePoints);
		line.setReferencePoint(new Point2D.Double(horizontalReference, 0.5));
		line.setHorizontalStretch(true);
		return line;
	}

	public static LayoutableNode makeVerticalLine(boolean stretch) {
		Point2D linePoints[] = { new Point(0, 0), new Point(0, 25) };
		PathNode line = new PathNode();
		line.setPathToPolyline(linePoints);
		line.setVerticalStretch(stretch);
		return line;
	}

	public static LayoutableNode makeCorner(CornerDirection direction) {
		PathNode corner = new PathNode();
		switch (direction) {
		case NORTHWEST:
			Point nwPoints[] = { new Point(10, 5), new Point(5, 5),
					new Point(5, 10) };
			corner.setPathToPolyline(nwPoints);
			corner.setReferencePoint(new Point(0, 0));
			break;
		case NORTHEAST:
			Point nePoints[] = { new Point(0, 5), new Point(5, 5),
					new Point(5, 10) };
			corner.setPathToPolyline(nePoints);
			corner.setReferencePoint(new Point(1, 0));
			break;
		case SOUTHWEST:
			Point swPoints[] = { new Point(5, 0), new Point(5, 5),
					new Point(10, 5) };
			corner.setPathToPolyline(swPoints);
			corner.setReferencePoint(new Point(0, 1));
			break;
		case SOUTHEAST:
			Point sePoints[] = { new Point(0, 5), new Point(5, 5),
					new Point(5, 0) };
			corner.setPathToPolyline(sePoints);
			corner.setReferencePoint(new Point(1, 1));
			break;
		}
		corner.setHorizontalStretch(true);
		corner.setVerticalStretch(true);
		return corner;
	}
	*/
	
	public static boolean intersects(Point2D point, Rectangle2D rectangle) {
		return point.getX() >= rectangle.getMinX()
				&& point.getX() <= rectangle.getMaxX()
				&& point.getY() >= rectangle.getMinY()
				&& point.getY() <= rectangle.getMaxY();
	}
}
