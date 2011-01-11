package nl.ru.icis.mbsd.itasks.gin.model.expressions;

import org.json.simple.JSONArray;

public class CleanExpression extends Expression {
	private String text;
	
	public CleanExpression() {
		text = "";
	}
	
	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
		setChanged();
	}

	public static CleanExpression fromJSON(String text) {
		CleanExpression result = new CleanExpression();
		result.setText(text);
		return result; 
	}
	
	@SuppressWarnings("unchecked")
	public JSONArray toJSON() {
		JSONArray result = new JSONArray();
		result.add("GCleanExpression");
		result.add(getText());
		return result;
	}

	@Override
	public int hashCode() {
		int result = (text == null) ? 0 : text.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof CleanExpression))
			return false;
		CleanExpression other = (CleanExpression) obj;
		if (text == null) {
			if (other.text != null)
				return false;
		} else if (!text.equals(other.text))
			return false;
		return true;
	}
}
