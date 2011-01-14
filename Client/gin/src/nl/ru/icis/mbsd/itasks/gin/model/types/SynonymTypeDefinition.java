package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class SynonymTypeDefinition extends TypeDefinition {
	private TypeExpressionContainer typeExpressionContainer;

	public TypeExpressionContainer getTypeExpressionContainer() {
		return typeExpressionContainer;
	}

	public void setTypeExpressionContainer(
			TypeExpressionContainer typeExpressionContainer) {
		typeExpressionContainer.setParent(this);
		this.typeExpressionContainer = typeExpressionContainer;
	}

	public static SynonymTypeDefinition fromJSON(Object tec, Scope scope) throws JSONException {
		SynonymTypeDefinition result = new SynonymTypeDefinition();
		result.setTypeExpressionContainer(TypeExpressionContainer.fromJSON(tec, scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = super.toJSON();
		JSONArray jsonSynonym = new JSONArray();
		jsonSynonym.add("GSynonymTypeRhs");
		jsonSynonym.add(getTypeExpressionContainer().toJSON());
		result.put("rhs", jsonSynonym);
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((typeExpressionContainer == null) ? 0
						: typeExpressionContainer.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		SynonymTypeDefinition other = (SynonymTypeDefinition) obj;
		if (typeExpressionContainer == null) {
			if (other.typeExpressionContainer != null)
				return false;
		} else if (!typeExpressionContainer
				.equals(other.typeExpressionContainer))
			return false;
		return true;
	}
}
