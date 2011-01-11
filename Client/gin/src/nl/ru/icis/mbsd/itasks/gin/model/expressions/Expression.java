package nl.ru.icis.mbsd.itasks.gin.model.expressions;

import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.Model;

import org.json.simple.JSONArray;

public abstract class Expression extends Model {
	private ExpressionContainer container;
	
	public Expression() {
	}
	
	public ExpressionContainer getContainer() {
		return container;
	}
	
	public void setContainer(ExpressionContainer container) {
		this.container = container;
	}
			
	public abstract JSONArray toJSON();
}
