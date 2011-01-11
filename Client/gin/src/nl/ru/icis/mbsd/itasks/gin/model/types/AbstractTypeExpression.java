package nl.ru.icis.mbsd.itasks.gin.model.types;

import org.json.simple.JSONArray;

public class AbstractTypeExpression extends TypeExpression {
	private static final long serialVersionUID = -1120520729178744108L;
	private String constructor; 

	public AbstractTypeExpression(String basicType) {
		this.constructor = basicType;
	}
	
	public AbstractTypeExpression(JSONArray jsonArray) {
		this.constructor = (String)jsonArray.get(1); 
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GBasicTypeExpression");
		result.add(constructor);
		return result;
	}

	@Override
	public String toString() {
		return constructor;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((constructor == null) ? 0 : constructor.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		AbstractTypeExpression other = (AbstractTypeExpression) obj;
		if (constructor == null) {
			if (other.constructor != null)
				return false;
		} else if (!constructor.equals(other.constructor))
			return false;
		return true;
	}
}
