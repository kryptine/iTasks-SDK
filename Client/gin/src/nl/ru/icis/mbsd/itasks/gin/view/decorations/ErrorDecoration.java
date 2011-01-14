package nl.ru.icis.mbsd.itasks.gin.view.decorations;

import java.awt.BasicStroke;
import java.awt.Color;

import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.view.DecoratableView;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.nodes.PImage;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.nodes.PText;
import edu.umd.cs.piccolo.util.PBounds;

public class ErrorDecoration extends Decoration {
	private static final long serialVersionUID = 8967360945894215922L;
	private Model model;
	private PPath errorPath;
	private PPath tooltip;
	private PImage tooltipIcon;
	private PText toolTipText;

	public ErrorDecoration(final DecoratableView decoratableView, final Model model) {
		super(decoratableView);
		this.model = model;
		
		errorPath = new PPath();
		errorPath.setPathToRectangle(0, 0, 1, 1);
		errorPath.setPickable(false);

		float dash[] = { 10.0f };
		errorPath.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER, 10.0f, dash, 0.0f));
		errorPath.setStroke(new BasicStroke(2.0f));
		errorPath.setStrokePaint(new Color(255, 0, 0));
		addChild(errorPath);

		tooltip = new PPath();
		tooltip.setPathToRectangle(0, 0, 1, 1);
		tooltip.setPaint(Color.cyan);
		tooltip.setVisible(false);
		addChild(tooltip);

		tooltipIcon = new PImage(ImageCache.getInstance().getImageFromName("exclamation"));
		tooltipIcon.setBounds(0, 0, 16, 16);
		tooltip.addChild(tooltipIcon);

		toolTipText = new PText();
		toolTipText.setOffset(16, 0);
		tooltip.addChild(toolTipText);
		
		decoratableView.addInputEventListener(new PBasicInputEventHandler() {
			public void mouseEntered(PInputEvent event) {
				if (model.getHintMessage() != null) {
					toolTipText.setText(model.getHintMessage());
					tooltip.setBounds(tooltip.getUnionOfChildrenBounds(null));
					tooltip.moveToFront();
					getDecoratableView().moveToFront();
					tooltip.setVisible(true);
					
					PBounds bounds = decoratableView.getBounds();
					tooltip.setOffset(bounds.getMaxX() + 5, 0);
				}
			}
			
			public void mouseExited(PInputEvent event) {
				tooltip.setVisible(false);
			}
		});
		update();
	}

	@Override
	public void update() {
		if (model.getHintMessage() != null) {
			PBounds b = getDecoratableView().getBounds(); 
			b.inset(-2, -2);
			errorPath.setBounds(b);
			errorPath.moveToFront();
			errorPath.setVisible(true);
		}
		else {
			errorPath.setVisible(false);
		}
	}

}
