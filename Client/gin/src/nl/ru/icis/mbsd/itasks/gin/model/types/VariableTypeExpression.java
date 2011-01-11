package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;

import org.json.simple.JSONArray;

public class VariableTypeExpression extends TypeExpression {
	private static final long serialVersionUID = 425578107640531677L;
	
	private String typeVariable; 

	public VariableTypeExpression() {
	}
	
	public VariableTypeExpression(String typeVariable) {
		this.typeVariable = typeVariable;
	}

	public static VariableTypeExpression fromJSON (JSONArray jsonArray) throws JSONException {
		VariableTypeExpression result = new VariableTypeExpression();
		result.typeVariable = (String) jsonArray.get(1);
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GTypeVariable");
		result.add(typeVariable);
		return result;
	}

	@Override
	public String toString() {
		return typeVariable;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((typeVariable == null) ? 0 : typeVariable.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof VariableTypeExpression))
			return false;
		VariableTypeExpression other = (VariableTypeExpression) obj;
		if (typeVariable == null) {
			if (other.typeVariable != null)
				return false;
		} else if (!typeVariable.equals(other.typeVariable))
			return false;
		return true;
	}
}
