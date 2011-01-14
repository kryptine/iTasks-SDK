package nl.ru.icis.mbsd.itasks.gin.model.types;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class DataConstructor extends Model {
	private String name;
	private ArrayList<TypeExpressionContainer> arguments = new ArrayList<TypeExpressionContainer>();
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
		setChanged();
	}

	public ArrayList<TypeExpressionContainer> getArguments() {
		return arguments;
	}

	public void addArgument(TypeExpressionContainer argument) {
		arguments.add(argument);
		argument.setParent(this);
		setChanged();
	}

	public static DataConstructor fromJSON(JSONObject jsonDataConstructor, Scope scope) throws JSONException {
		DataConstructor result = new DataConstructor();
		result.setName((String) jsonDataConstructor.get("name"));
		JSONArray arguments = (JSONArray) jsonDataConstructor.get("arguments");
		for (Object o: arguments)
			result.addArgument(TypeExpressionContainer.fromJSON(o, scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		JSONArray jsonArguments = new JSONArray();
		for (TypeExpressionContainer argument: getArguments())
			jsonArguments.add(argument.toJSON());
		result.put("name", getName());
		result.put("arguments", jsonArguments);
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((arguments == null) ? 0 : arguments.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (getClass() != obj.getClass())
			return false;
		DataConstructor other = (DataConstructor) obj;
		if (arguments == null) {
			if (other.arguments != null)
				return false;
		} else if (!arguments.equals(other.arguments))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}
