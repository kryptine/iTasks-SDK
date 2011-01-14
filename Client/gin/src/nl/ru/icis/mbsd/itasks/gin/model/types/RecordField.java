package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONObject;

public class RecordField extends Model {
	private String name;
	private TypeExpressionContainer typeExpressionContainer;
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	
	public TypeExpressionContainer getTypeExpressionContainer() {
		return typeExpressionContainer;
	}
	
	public void setTypeExpressionContainer(
		TypeExpressionContainer typeExpressionContainer) {
		typeExpressionContainer.setParent(this);
		this.typeExpressionContainer = typeExpressionContainer;
	}
	
	public static RecordField fromJSON(JSONObject field, Scope scope) throws JSONException {
		RecordField result = new RecordField();
		result.setName((String) field.get("name"));
		result.setTypeExpressionContainer(TypeExpressionContainer.fromJSON(field.get("type"), scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		result.put("name", getName());
		result.put("type", getTypeExpressionContainer().toJSON());
		return result;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		if (getClass() != obj.getClass())
			return false;
		RecordField other = (RecordField) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (typeExpressionContainer == null) {
			if (other.typeExpressionContainer != null)
				return false;
		} else if (!typeExpressionContainer
				.equals(other.typeExpressionContainer))
			return false;
		return true;
	}
}
