package nl.ru.icis.mbsd.itasks.gin.view;

import nl.ru.icis.mbsd.itasks.gin.view.decorations.Decoration;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.util.PBounds;

public abstract class DecoratableView extends PNode implements View{
	private static final long serialVersionUID = 6840201889808148982L;
	
	private PNode decorations;
	
	public DecoratableView() {
		decorations = new PNode();
		addChild(decorations);
	}
	
	public void addDecoration(Decoration decoration)
	{
		decorations.addChild(decoration);
	}
	
	@Override
	public PBounds getUnionOfChildrenBounds(PBounds dstBounds) {
		//Calculate children bounds, except decorations
		if (dstBounds == null) {
			dstBounds = new PBounds();
		} else {
			dstBounds.resetToZero();
		}
		
		int count = getChildrenCount();
		for (int i = 0; i < count; i++) {
			PNode each = (PNode) getChildrenReference().get(i);
			if (each == decorations)
				continue;
			dstBounds.add(each.getFullBoundsReference());
		}
		
		return dstBounds;
	}
	
	protected void updateDecorations() {
		int count = decorations.getChildrenCount();
		for (int i = 0; i < count; i++)
			((Decoration) decorations.getChildrenReference().get(i)).update();
	}
	
	public void updateView(Object source) {
		updateDecorations();
	}

	@Override
	protected void layoutChildren() {
		setBounds(getUnionOfChildrenBounds(null));
		updateDecorations();
	}
}
