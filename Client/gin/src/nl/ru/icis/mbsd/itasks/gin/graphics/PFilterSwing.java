package nl.ru.icis.mbsd.itasks.gin.graphics;

import javax.swing.JComponent;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.PLayer;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEventFilter;
import edu.umd.cs.piccolo.util.PPaintContext;
import edu.umd.cs.piccolox.pswing.PSwing;
import edu.umd.cs.piccolox.pswing.PSwingCanvas;
import edu.umd.cs.piccolox.pswing.PSwingEventHandler;

public class PFilterSwing extends PNode{
	private static final long serialVersionUID = -1883210431873385820L;
	
	private PSwingCanvas swingCanvas;
	private PSwing swing;

	private PInputEventFilter filter; 
	
	public PFilterSwing(JComponent component) {
		PBasicInputEventHandler handler = new PBasicInputEventHandler();
		filter = new PInputEventFilter();
		handler.setEventFilter(filter);
		addInputEventListener(handler);
		swing = new PSwing(component);
		addChild(swing);
	}
	
	public boolean getHandleEvents() {
		return filter.getMarksAcceptedEventsAsHandled();
	}
	
	public void setHandleEvents(boolean b) {
		filter.setMarksAcceptedEventsAsHandled(b);
	}
	
	@Override
	protected void paint(PPaintContext arg0) {
		if (swingCanvas == null) {
			swingCanvas = getSwingCanvas();
			if (swingCanvas != null)
				swing.addInputEventListener(new PSwingEventHandler(swingCanvas, swing));
		}

		super.paint(arg0);
	}

    private PSwingCanvas getSwingCanvas() {
        PNode p = this;
        while (p != null) {
            final PNode parent = p;
            if (parent instanceof PLayer) {
                final PLayer player = (PLayer) parent;
                for (int i = 0; i < player.getCameraCount(); i++) {
                    final PCamera cam = player.getCamera(i);
                    if (cam.getComponent() instanceof PSwingCanvas) {
                    	return (PSwingCanvas) cam.getComponent();
                    }
                }
            }
            p = p.getParent();
        }
        return null;
    }
    
	@Override
	protected void layoutChildren() {
		setBounds(swing.getBounds());
	}
}
