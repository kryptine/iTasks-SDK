package nl.ru.icis.mbsd.itasks.gin.graphics;

import edu.umd.cs.piccolo.PNode;

public abstract class LayoutNode extends LayoutableNode {
	private static final long serialVersionUID = -8303183586925518256L;
	
	private void checkLayoutable(PNode child) {
		if (!(child instanceof LayoutableNode)) {
			IllegalArgumentException e = new IllegalArgumentException(child.toString() + " is not a member of LayoutableNode");
			e.printStackTrace();
			throw e;
		}
	}
	
	@Override
	public void addChild(int index, PNode child) {
		checkLayoutable(child);
		super.addChild(index, child);
	}


	@Override
	public void addChild(PNode child) {
		checkLayoutable(child);
		super.addChild(child);
	}
}
