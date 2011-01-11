package nl.ru.icis.mbsd.itasks.gin.model.types;

import org.json.simple.JSONArray;

public class BasicTypeExpression extends TypeExpression {
	private static final long serialVersionUID = -1120520729178744108L;
	private String basicType; 

	public BasicTypeExpression(String basicType) {
		this.basicType = basicType;
	}
	
	public BasicTypeExpression(JSONArray jsonArray) {
		this.basicType = (String)jsonArray.get(1); 
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GBasicTypeExpression");
		result.add(basicType);
		return result;
	}

	@Override
	public String toString() {
		return basicType;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((basicType == null) ? 0 : basicType.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		BasicTypeExpression other = (BasicTypeExpression) obj;
		if (basicType == null) {
			if (other.basicType != null)
				return false;
		} else if (!basicType.equals(other.basicType))
			return false;
		return true;
	}
}
