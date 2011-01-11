package nl.ru.icis.mbsd.itasks.gin;

import java.awt.Point;
import java.awt.datatransfer.DataFlavor;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.geom.Point2D;
import java.util.Iterator;

import nl.ru.icis.mbsd.itasks.gin.graphics.PathUtils;
import nl.ru.icis.mbsd.itasks.gin.model.Declaration;
import nl.ru.icis.mbsd.itasks.gin.view.ModuleView;
import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.PNode;

public class DragAndDropHandler implements DropTargetListener {

	private DragAndDropTarget currentTarget = null;

	private ModuleView source;

	private PCanvas target;

	public DragAndDropHandler(ModuleView source, PCanvas canvas) {
		this.source = source;
		this.target = canvas;
	}

	@Override
	public void dragEnter(DropTargetDragEvent dtde) {
		dtde.acceptDrag(DnDConstants.ACTION_MOVE);
	}

	@Override
	public void dragExit(DropTargetEvent dte) {
	}

	@Override
	public void dragOver(DropTargetDragEvent dtde) {
		dtde.acceptDrag(DnDConstants.ACTION_MOVE);
		
		if (dtde.getTransferable().isDataFlavorSupported(
				DataFlavor.stringFlavor)) {
			DragAndDropTarget newTarget = findTarget(dtde.getLocation());
			if (newTarget != null && newTarget.acceptDrag(dtde.getLocation())) {
				dtde.acceptDrag(DnDConstants.ACTION_COPY);
				if (currentTarget != null && currentTarget != newTarget)
					currentTarget.dragExit();
				newTarget.dragEnter(dtde.getLocation());
				currentTarget = newTarget;
			} else {
				dtde.rejectDrag();
			}
		}
	}

	@Override
	public void drop(DropTargetDropEvent dtde) {
		try {
			if (dtde.getTransferable().isDataFlavorSupported(
					DataFlavor.stringFlavor)) {
				dtde.acceptDrop(DnDConstants.ACTION_COPY);
				DragAndDropTarget newTarget = findTarget(dtde.getLocation());
				if (newTarget != null
						&& newTarget.acceptDrag(dtde.getLocation())) {
					Declaration selection = source.getSelection();
					if (selection != null) 
						newTarget.drop(selection, dtde.getLocation());
					currentTarget = null;
					dtde.getDropTargetContext().dropComplete(true);
				} else {
					dtde.rejectDrop();
				}
			} else {
				dtde.rejectDrop();
			}
		} catch (InvalidDnDOperationException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void dropActionChanged(DropTargetDragEvent dtde) {
	}

	private DragAndDropTarget findTarget(Point point) {
		return (DragAndDropTarget) findNodeRec(target.getCamera().localToView(
				point), target.getLayer());
	}

	private static PNode findNodeRec(Point2D point, PNode node) {
		@SuppressWarnings("rawtypes")
		Iterator i = node.getChildrenIterator();
		while (i.hasNext()) {
			PNode next = (PNode) i.next();
			if (PathUtils.intersects(point, next.getGlobalFullBounds())) {
				PNode result = findNodeRec(point, next);
				if (result != null)
					return result;
				else if (next instanceof DragAndDropTarget)
					return next;
			}
		}
		return null;
	}
}
