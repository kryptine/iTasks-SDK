package nl.ru.icis.mbsd.itasks.gin.view.expressions;

import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.Expression;
import nl.ru.icis.mbsd.itasks.gin.view.DecoratableView;
import nl.ru.icis.mbsd.itasks.gin.view.Selectable;
import nl.ru.icis.mbsd.itasks.gin.view.View;
import nl.ru.icis.mbsd.itasks.gin.view.decorations.ErrorDecoration;
import nl.ru.icis.mbsd.itasks.gin.view.decorations.SelectionDecoration;
public abstract class ExpressionView extends DecoratableView implements View, Selectable {
	private static final long serialVersionUID = -5129177668214824846L;

	private Expression expression;

	protected ExpressionView(Expression expression) {
		expression.addView(this);
		this.expression = expression;
	}
	
	protected void addDefaultDecorations() {
		addDecoration(new SelectionDecoration(this, true));
		addDecoration(new ErrorDecoration(this, getExpression()));
	}

	public Expression getExpression() {
		return expression;
	}
	
	@Override
	public void setSelected(boolean selected) {
		updateDecorations();
	}
	
	@Override
	public void Remove() {
		ExpressionContainer container = getExpression().getContainer();
		if (container != null) {
		container.clear();
		container.notifyViews();
		}
	}
}
