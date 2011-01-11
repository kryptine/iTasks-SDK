package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;

public class ListTypeExpression extends TypeExpression {
	private static final long serialVersionUID = -1890293757860626811L;
	private TypeExpressionContainer elementType; 
	
	public ListTypeExpression() {
	}
	
	public TypeExpressionContainer getElementType() {
		return elementType;
	}
	
	public void setElementType (TypeExpressionContainer elementType) {
		this.elementType = elementType;
	}
	
	public static ListTypeExpression fromJSON (JSONArray jsonArray, Scope scope) throws JSONException {
		ListTypeExpression result = new ListTypeExpression();
		result.setElementType(TypeExpressionContainer.fromJSON(jsonArray.get(1), scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GList");
		result.add(elementType.toJSON());
		return result;
	}
	
	@Override
	public String toString() {
		return "[" + elementType.toString() + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((elementType == null) ? 0 : elementType.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ListTypeExpression))
			return false;
		ListTypeExpression other = (ListTypeExpression) obj;
		if (elementType == null) {
			if (other.elementType != null)
				return false;
		} else if (!elementType.equals(other.elementType))
			return false;
		return true;
	}
}
