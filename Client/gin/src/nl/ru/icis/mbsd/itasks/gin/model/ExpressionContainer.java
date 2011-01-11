package nl.ru.icis.mbsd.itasks.gin.model;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.StringTokenizer;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.CleanExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.Expression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.ListComprehensionExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.ListExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.AbstractTypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.ApplicationTypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.ListTypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeExpressionContainer;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class ExpressionContainer extends Model {
	private TypeExpressionContainer type;
	private Expression expression;
	
	public ExpressionContainer() {
	}
	
	public TypeExpressionContainer getTypeExpressionContainer() {
		return type;
	}
	
	public void setTypeExpressionContainer(TypeExpressionContainer type) {
		this.type = type;
		type.setParent(this);
		setChanged();
	}
	
	public Expression getExpression() {
		return expression;
	}
	
	public void setExpression(Expression expression) {
		if (this.expression != null)
			this.expression.setParent(null);
		
		this.expression = expression;
		if (expression != null) {
			expression.setParent(this);
			expression.setContainer(this);
		}
		setChanged();
	}

	public void clear() {
		setExpression(null);
		setChanged();
	}
	
	public static ExpressionContainer fromJSON(Object jsonContainer, TypeExpressionContainer type, Scope scope)
	throws JSONException {
		ExpressionContainer result = new ExpressionContainer();
		result.setTypeExpressionContainer(type);
		
		if (jsonContainer instanceof String && jsonContainer.equals("GUndefinedExpression")) {
			result.setExpression(null);
			return result;
		}
	
		JSONArray jsonArray = (JSONArray) jsonContainer;
		String expressionType = (String) jsonArray.get(0);
		if (expressionType.equals("GGraphExpression"))
			result.setExpression(GraphExpression.fromJSON((JSONObject)jsonArray.get(1), scope));
		else if (expressionType.equals("GListExpression")) { 
			if (type.getTypeExpression() instanceof ListTypeExpression)
				result.setExpression(ListExpression.fromJSON((JSONArray)jsonArray.get(1), (ListTypeExpression) (type.getTypeExpression()), scope));
			else 
				throw new JSONException ("GListExpression specified but type is not a list");
		}
		else if (expressionType.equals("GListComprehensionExpression")) { 
			if (type.getTypeExpression() instanceof ListTypeExpression)
				result.setExpression(ListComprehensionExpression.fromJSON((JSONObject)jsonArray.get(1), (ListTypeExpression) (type.getTypeExpression()), scope));
			else 
				throw new JSONException ("GListComprehensionExpression specified but type is not a list");
		}
		else if (expressionType.equals("GCleanExpression")) 
			result.setExpression(CleanExpression.fromJSON((String)jsonArray.get(1)));
		else
			throw new JSONException("Unknown expression type:" + expressionType);
		return result;
	}
	
	public Object toJSON() {
		if (expression != null)
			return expression.toJSON();
		else
			return "GUndefinedExpression";
	}
	
	public ArrayList<Class <? extends Expression>> getSupportedExpressions() {
		ArrayList<Class<? extends Expression>> result = new ArrayList<Class<? extends Expression>>();		
		if (type.getTypeExpression() instanceof ApplicationTypeExpression) {
			ApplicationTypeExpression ate = (ApplicationTypeExpression)type.getTypeExpression();
			if (ate.getA().getTypeExpression() != null && ate.getA().getTypeExpression() instanceof AbstractTypeExpression) {
				AbstractTypeExpression bte = (AbstractTypeExpression)(ate.getA().getTypeExpression());
				if (bte.toString().equals("Task"))
					result.add(GraphExpression.class);
			}
		}
		if (type.getTypeExpression() instanceof ListTypeExpression) {
			result.add(ListExpression.class);
			result.add(ListComprehensionExpression.class);
		}
		result.add(CleanExpression.class);
		return result;
	}
	
	@Override
	protected void setErrorFromPath(Object source, StringTokenizer st, String message)
			throws SecurityException, NoSuchMethodException,
			IllegalArgumentException, IllegalAccessException,
			InvocationTargetException {
		if (expression == null) {
			if (st.hasMoreTokens())
				throw new RuntimeException("Invalid path: expression not found");
			else {
				setErrorMessage(source, message);
			}
		}
		else
			expression.setErrorFromPath(source, st, message);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((expression == null) ? 0 : expression.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ExpressionContainer))
			return false;
		ExpressionContainer other = (ExpressionContainer) obj;
		if (expression == null) {
			if (other.expression != null)
				return false;
		} else if (!expression.equals(other.expression))
			return false;
		return true;
	}
}
