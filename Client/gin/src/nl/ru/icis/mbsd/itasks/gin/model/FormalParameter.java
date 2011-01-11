package nl.ru.icis.mbsd.itasks.gin.model;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeExpressionContainer;

import org.json.simple.JSONObject;

public class FormalParameter extends Model {
	private static final long serialVersionUID = 7577434084571496977L;
	
	private String name;
	private TypeExpressionContainer type;
	
	public FormalParameter(Scope scope)
	{
		name = "";
		type = new TypeExpressionContainer();
	}

	public String toString() {
		return name;// + " :: " + type.toString(); 
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
		setChanged();
	}

	public TypeExpressionContainer getType() {
		return type;
	}

	private void setType(TypeExpressionContainer type) {
		this.type.setParent(null);
		this.type = type;
		type.setParent(this);
		setChanged();
	}
	
	public static FormalParameter fromJSON(JSONObject jsonFormalParameter, Scope scope) throws JSONException {
		FormalParameter result = new FormalParameter(scope);
		result.setName((String) jsonFormalParameter.get("name"));
		result.setType(TypeExpressionContainer.fromJSON(jsonFormalParameter.get("type"), scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		result.put("name", getName());
		result.put("type", getType().toJSON());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof FormalParameter))
			return false;
		FormalParameter other = (FormalParameter) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}
}
