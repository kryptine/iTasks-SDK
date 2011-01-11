package nl.ru.icis.mbsd.itasks.gin.model.expressions;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;
import nl.ru.icis.mbsd.itasks.gin.model.types.ListTypeExpression;

import org.json.simple.JSONArray;

public class ListExpression extends Expression {
	private ArrayList<ExpressionContainer> items;
	
	public ListExpression() {
		items = new ArrayList<ExpressionContainer>();
	}
	
	public ArrayList<ExpressionContainer> getItems() {
		return items; 
	}
	
	public void addExpressionContainer(ExpressionContainer exc) {
		items.add(exc);
		exc.setParent(this);
		setChanged();
	}
	
	public void addExpressionContainer(int index) {
		ExpressionContainer exc = new ExpressionContainer();
		exc.setTypeExpressionContainer(((ListTypeExpression)getContainer().getTypeExpressionContainer().getTypeExpression()).getElementType());
		items.add(index, exc);
		exc.setParent(this);
		setChanged();
	}	
	
	public void removeExpression(ExpressionContainer exc) {
		exc.setParent(null);
		items.remove(exc);
		setChanged();
	}
	
	public static Expression fromJSON(JSONArray jsonArray, ListTypeExpression type, Scope scope) throws JSONException {
		ListExpression result = new ListExpression();
		for (Object item: jsonArray) {
			result.addExpressionContainer(ExpressionContainer.fromJSON(item, type.getElementType(), scope));
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public JSONArray toJSON() {
		JSONArray result = new JSONArray();
		result.add("GListExpression");
		JSONArray jsonListExpression = new JSONArray();
		result.add(jsonListExpression);
		
		for (ExpressionContainer exp: getItems()) {
			jsonListExpression.add(exp.toJSON());
		}
		return result;
	}

	@Override
	public int hashCode() {
		int result = (items == null) ? 0 : items.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ListExpression))
			return false;
		ListExpression other = (ListExpression) obj;
		if (items == null) {
			if (other.items != null)
				return false;
		} else if (!items.equals(other.items))
			return false;
		return true;
	}
}
