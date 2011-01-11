package nl.ru.icis.mbsd.itasks.gin.view.decorations;

import java.awt.BasicStroke;
import java.awt.Color;

import nl.ru.icis.mbsd.itasks.gin.view.DecoratableView;
import nl.ru.icis.mbsd.itasks.gin.view.Selectable;
import nl.ru.icis.mbsd.itasks.gin.view.Selection;
import nl.ru.icis.mbsd.itasks.gin.view.expressions.TextExpressionView;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;

public class SelectionDecoration extends Decoration {
	private static final long serialVersionUID = 8681626410205549246L;
	private PPath highlightPath;
	
	public SelectionDecoration(DecoratableView decoratableView, final boolean allowSelection) {
		super(decoratableView);
		
		highlightPath = new PPath();
		highlightPath.setPathToRectangle(0, 0, 1, 1);
		highlightPath.setPickable(false);
		highlightPath.setStroke(new BasicStroke(2.0f));
		highlightPath.setStrokePaint(new Color(0, 0, 255));
		addChild(highlightPath);

		getDecoratableView().addInputEventListener(new PBasicInputEventHandler() {
			@Override
			public void mouseClicked(PInputEvent e) {
				super.mouseClicked(e);
				Selection.getInstance().set(allowSelection ? (Selectable)getDecoratableView() : null);
				e.getInputManager().setKeyboardFocus(this);
				super.mouseClicked(e);
				e.setHandled(true);
			}
			
			@Override
			public void keyTyped(PInputEvent e) {
				super.keyTyped(e);
				if ((e.getKeyChar() == 8 || e.getKeyChar() == 127)
					&& Selection.getInstance().get() != null
					&& ! (getDecoratableView() instanceof TextExpressionView)) {
					Selection.getInstance().removeSelection();
					e.setHandled(true);
				}
			}
		});
		update();
	}
	
	@Override
	public void update() {
		if (Selection.getInstance().get() == getDecoratableView()) {
			PBounds b = getDecoratableView().getUnionOfChildrenBounds(null); 
			b.inset(-2, -2);
			highlightPath.setBounds(b);
			highlightPath.setVisible(true);
		}
		else {
			highlightPath.setVisible(false);
		}
	}
}
