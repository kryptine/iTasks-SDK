package nl.ru.icis.mbsd.itasks.gin;

import java.awt.geom.Point2D;

import nl.ru.icis.mbsd.itasks.gin.model.Declaration;

public interface DragAndDropTarget {
	boolean acceptDrag(Point2D position);
	void dragEnter(Point2D position); 
	void dragExit();
	void drop(Declaration selection, Point2D position);
}
