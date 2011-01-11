package nl.ru.icis.mbsd.itasks.gin.view.decorations;

import nl.ru.icis.mbsd.itasks.gin.view.DecoratableView;
import edu.umd.cs.piccolo.PNode;

public abstract class Decoration extends PNode {
	private static final long serialVersionUID = -5498701203901772006L;
	
	private DecoratableView decoratableView;
	
	public Decoration(DecoratableView decoratableView){
		this.decoratableView = decoratableView;
	}

	protected DecoratableView getDecoratableView() {
		return decoratableView;
	}
	
	public abstract void update();
}
