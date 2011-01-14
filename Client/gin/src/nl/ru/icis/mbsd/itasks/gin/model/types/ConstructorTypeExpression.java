package nl.ru.icis.mbsd.itasks.gin.model.types;

import org.json.simple.JSONArray;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

public class ConstructorTypeExpression extends TypeExpression {
	private TypeDefinition typeDefinition;
	
	public ConstructorTypeExpression()
	{
	}
	
	public ConstructorTypeExpression(TypeDefinition typeDefinition)
	{
		this.typeDefinition = typeDefinition;
	}
	
	public TypeDefinition getTypeDefinition() {
		return typeDefinition;
	}

	public void setTypeDefinition(TypeDefinition typeDefinition) {
		this.typeDefinition = typeDefinition;
		typeDefinition.setParent(this);
		setChanged();
	}
	
	public static ConstructorTypeExpression fromJSON (JSONArray jsonArray, Scope scope) throws JSONException {
		ConstructorTypeExpression result = new ConstructorTypeExpression();
		String constructorName = (String)jsonArray.get(1);
		TypeDefinition td = scope.getTypeScope().get(constructorName);
		if (td == null) {
			System.err.println("Warning: type \"" + constructorName + "\" undefined");
			td = new TypeDefinition();
			td.setName(constructorName);
		}
		result.setTypeDefinition(td);
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GConstructor");
		result.add(getTypeDefinition().getName());
		return result;
	}
		
	@Override
	public String toString() {
			return getTypeDefinition().getName();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((typeDefinition == null) ? 0 : typeDefinition.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ConstructorTypeExpression))
			return false;
		ConstructorTypeExpression other = (ConstructorTypeExpression) obj;
		if (typeDefinition == null) {
			if (other.typeDefinition != null)
				return false;
		} else if (!typeDefinition.equals(other.typeDefinition))
			return false;
		return true;
	}
}
