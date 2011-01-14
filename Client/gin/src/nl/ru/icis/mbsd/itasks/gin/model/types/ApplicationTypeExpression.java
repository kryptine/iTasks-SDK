package nl.ru.icis.mbsd.itasks.gin.model.types;

import org.json.simple.JSONArray;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

public class ApplicationTypeExpression extends TypeExpression {
	private TypeExpressionContainer a;
	private TypeExpressionContainer b;
	
	public ApplicationTypeExpression() {
	}

	public TypeExpressionContainer getA() {
		return a;
	}

	public void setA(TypeExpressionContainer a) {
		this.a = a;
		a.setParent(this);
		setChanged();
	}

	public TypeExpressionContainer getB() {
		return b;
	}

	public void setB(TypeExpressionContainer b) {
		this.b = b;
		b.setParent(this);
		setChanged();
	}

	public static ApplicationTypeExpression fromJSON (JSONArray jsonArray, Scope scope) throws JSONException {
		ApplicationTypeExpression result = new ApplicationTypeExpression();
		result.setA(TypeExpressionContainer.fromJSON(jsonArray.get(1), scope)); 
		result.setB(TypeExpressionContainer.fromJSON(jsonArray.get(2), scope)); 
		return result;
	}
		
	@SuppressWarnings("unchecked")
	@Override
	public Object toJSON() {
		JSONArray result = new JSONArray();
		result.add("GTypeApplication");
		result.add(getA().toJSON());
		result.add(getB().toJSON());
		return result;
	}
		
	@Override
	public String toString() {
			String a = getA().toString();
			String b = getB().toString();
			if (b.contains(" " ))
				b = "(" + b + ")"; 
			return a + " " + b;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((a == null) ? 0 : a.hashCode());
		result = prime * result + ((b == null) ? 0 : b.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ApplicationTypeExpression))
			return false;
		ApplicationTypeExpression other = (ApplicationTypeExpression) obj;
		if (a == null) {
			if (other.a != null)
				return false;
		} else if (!a.equals(other.a))
			return false;
		if (b == null) {
			if (other.b != null)
				return false;
		} else if (!b.equals(other.b))
			return false;
		return true;
	}
}
