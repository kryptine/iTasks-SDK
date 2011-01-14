package nl.ru.icis.mbsd.itasks.gin.model.types;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;

public class TupleTypeExpression extends TypeExpression {
	private static final long serialVersionUID = -7437229885413584463L;
	private ArrayList<TypeExpressionContainer> elementTypes = new ArrayList<TypeExpressionContainer>();
	
	public TupleTypeExpression() {
	}
	
	public ArrayList<TypeExpressionContainer> getElementTypes() {
		return elementTypes;
	}
	
	public void addElementType (TypeExpressionContainer elementType) {
		elementType.setParent(this);
		elementTypes.add(elementType);
	}
	
	public static TupleTypeExpression fromJSON (JSONArray jsonArray, Scope scope) throws JSONException {
		TupleTypeExpression result = new TupleTypeExpression();
		for (Object o: (JSONArray)jsonArray.get(1)) {
			result.addElementType(TypeExpressionContainer.fromJSON(o, scope));
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GTuple");
		JSONArray jsonElements = new JSONArray();
		result.add(jsonElements);
		for (TypeExpressionContainer type: elementTypes)
			jsonElements.add(type.toJSON());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((elementTypes == null) ? 0 : elementTypes.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof TupleTypeExpression))
			return false;
		TupleTypeExpression other = (TupleTypeExpression) obj;
		if (elementTypes == null) {
			if (other.elementTypes != null)
				return false;
		} else if (!elementTypes.equals(other.elementTypes))
			return false;
		return true;
	}
}
