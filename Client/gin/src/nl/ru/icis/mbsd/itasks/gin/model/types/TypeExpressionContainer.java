package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;

public class TypeExpressionContainer extends Model {
	private TypeExpression typeExpression;
	
	public TypeExpressionContainer() {
	}
	
	public TypeExpressionContainer(TypeExpression typeExpression) {
		setTypeExpression(typeExpression);
	}
	
	public TypeExpression getTypeExpression() {
		return typeExpression;
	}
	
	public void setTypeExpression(TypeExpression typeExpression) {
		if (this.typeExpression != null)
			this.typeExpression.setParent(null);
		
		this.typeExpression = typeExpression;
		if (typeExpression != null) {
			typeExpression.setParent(this);
		}
		setChanged();
	}

	public void clear() {
		setTypeExpression(null);
		setChanged();
	}
	
	public static TypeExpressionContainer fromJSON (Object jsonType, Scope scope) throws JSONException {
		TypeExpressionContainer result = new TypeExpressionContainer();
		
		String constructorName = (String) (jsonType instanceof String ? jsonType : ((JSONArray)jsonType).get(0));
		if (constructorName.equals("GUndefinedTypeExpression")) {
			result.setTypeExpression(null);
		} else if (constructorName.equals("GBasicTypeExpression")) {
			result.setTypeExpression(new BasicTypeExpression((JSONArray)jsonType));
		} else if (constructorName.equals("GAbstractTypeExpression")) {
			result.setTypeExpression(new AbstractTypeExpression((JSONArray)jsonType));
		} else if (constructorName.equals("GList")) {
			result.setTypeExpression(ListTypeExpression.fromJSON((JSONArray)jsonType, scope));
		} else if (constructorName.equals("GTuple")) {
			result.setTypeExpression(TupleTypeExpression.fromJSON((JSONArray)jsonType, scope));
		} else if (constructorName.equals("GConstructor")) {
			result.setTypeExpression(ConstructorTypeExpression.fromJSON((JSONArray)jsonType, scope));
		} else if (constructorName.equals("GTypeApplication")) {
			result.setTypeExpression(ApplicationTypeExpression.fromJSON((JSONArray)jsonType, scope));
		} else if (constructorName.equals("GTypeVariable")) {
			result.setTypeExpression(VariableTypeExpression.fromJSON((JSONArray)jsonType));
		} else {
			throw new JSONException("Unknown type: " + constructorName);
		}
		return result;
	}

	public Object toJSON() {
		if (typeExpression != null)
			return typeExpression.toJSON();
		else
			return "GUndefinedTypeExpression";
	}

	@Override
	public String toString() {
		if (typeExpression != null)
			return typeExpression.toString();
		else
			return "<<Undefined type>>";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((typeExpression == null) ? 0 : typeExpression.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof TypeExpressionContainer))
			return false;
		TypeExpressionContainer other = (TypeExpressionContainer) obj;
		if (typeExpression == null) {
			if (other.typeExpression != null)
				return false;
		} else if (!typeExpression.equals(other.typeExpression))
			return false;
		return true;
	}
}
