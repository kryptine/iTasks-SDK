package nl.ru.icis.mbsd.itasks.gin.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class Definition extends Declaration implements Scope, DefinitionContainer {
	private static final long serialVersionUID = 5497191389694497407L;
	private ArrayList<Definition> localDefinitions;
	private ExpressionContainer body;
	
	public Definition(DefinitionContainer parent) {
		super(parent);
		localDefinitions = new ArrayList<Definition>();
		body = new ExpressionContainer();
	}

	public ArrayList<Definition> getLocals() {
		return localDefinitions;
	}
	
	@Override
	public void addLocal(Definition localDefinition) {
		localDefinitions.add(localDefinition);
		localDefinition.setParent(this);
		setChanged();
	}
	
	@Override
	public void removeLocal(Definition definition) {
		definition.setParent(null);
		localDefinitions.remove(definition);
		setChanged();
	}
	
	public ExpressionContainer getBody() {
		return body;
	}

	private void setBody(ExpressionContainer body) {
		this.body.setParent(null);
		this.body = body;
		body.setParent(this);
		setChanged();
	}
	
	@Override
	public Map<String, Declaration> getScope() {
		HashMap<String, Declaration> result = new HashMap<String, Declaration>();
		result.putAll(parentScope.getScope());
		for (Definition l : localDefinitions)
			result.put(l.getName(), l);
		return result;
	}

	public Declaration lookup(String name) {
		return getScope().get(name);
	}
	
	public void updateFromJSON(JSONObject jsonDefinition) throws JSONException {
		JSONArray jsonLocal = (JSONArray) jsonDefinition.get("locals");

		//Definitions can have mutual dependencies. First create all Definition objects...
		for (Object o : jsonLocal) {
			Definition d = Definition.fromJSON((JSONObject) o, this);
			d.setParent(this);
			addLocal(d);
		}
		//Then parse their json Definitions
		for (int i = 0; i < jsonLocal.size(); i++)
			getLocals().get(i).updateFromJSON((JSONObject) jsonLocal.get(i));
		
		setBody(ExpressionContainer.fromJSON(jsonDefinition.get("body"), getType(), this));
	}
	
	public static Definition fromJSON(JSONObject jsonDefinition, DefinitionContainer parent) throws JSONException {
		Definition result = new Definition(parent);
		result.setFromJSON((JSONObject) jsonDefinition.get("declaration"));
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		result.put("declaration", super.toJSON());
		JSONArray jsonLocal = new JSONArray();
		result.put("locals", jsonLocal);
		for (Definition d : getLocals())
			jsonLocal.add(d.toJSON());
		result.put("body", getBody().toJSON());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((body == null) ? 0 : body.hashCode());
		result = prime
				* result
				+ ((localDefinitions == null) ? 0 : localDefinitions.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (! super.equals(obj)) 
			return false;
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Definition))
			return false;
		Definition other = (Definition) obj;
		if (body == null) {
			if (other.body != null)
				return false;
		} else if (!body.equals(other.body))
			return false;
		if (localDefinitions == null) {
			if (other.localDefinitions != null)
				return false;
		} else if (!localDefinitions.equals(other.localDefinitions))
			return false;
		return true;
	}
}
