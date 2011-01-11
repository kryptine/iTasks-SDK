package nl.ru.icis.mbsd.itasks.gin.model;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.json.JSONUtils;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeDefinition;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

public class Module extends Model implements Scope, DefinitionContainer {
	private String name;
	private ArrayList<Import> imports;
	private ArrayList<TypeDefinition> types;
	private ArrayList<Definition> definitions;
	
	public Module() {
		this.imports = new ArrayList<Import>();
		this.types = new ArrayList<TypeDefinition>();
		this.definitions = new ArrayList<Definition>();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
		setChanged();
	}

	public ArrayList<Import> getImports() {
		return imports;
	}

	public void addImport(Import imp) {
		imp.setParent(this);
		imports.add(imp);
		setChanged();
	}
	
	public ArrayList<TypeDefinition> getTypeDefinitions() {
		return types;
	}

	public void addTypeDefinition(TypeDefinition definition) {
		types.add(definition);
		definition.setParent(this);
		setChanged();
	}
	
	public ArrayList<Definition> getDefinitions() {
		return definitions;
	}

	public void addLocal(Definition definition) {
		definitions.add(definition);
		definition.setParent(this);
		setChanged();
	}

	@Override
	public void removeLocal(Definition definition) {
		definition.setParent(null);
		definitions.remove(definition);
		setChanged();
	}

	@Override
	public Map<String, Declaration> getScope() {
		HashMap<String, Declaration> result = new HashMap<String, Declaration>();
		for (Import i: imports)
			result.putAll(i.getScope());
		for (Definition d : definitions)
			result.put(d.getName(), d);
		return result;
	}

	@Override
	public Map<String, TypeDefinition> getTypeScope() {
		HashMap<String, TypeDefinition> result = new HashMap<String, TypeDefinition>();
		for (Import i: imports)
			result.putAll(i.getTypeScope());
		for (TypeDefinition t: types)
			result.put(t.getName(), t);
		return result;
	}
	
	public static Module fromJSON(JSONObject jsonModule) throws JSONException {
		Module result = new Module();
		result.setName(((String) jsonModule.get("name")));
		JSONArray jsonImports = (JSONArray) jsonModule.get("imports");
		for (Object o : jsonImports)
			result.addImport((Import.fromJSON((JSONObject) o)));
		JSONArray jsonTypes = (JSONArray) jsonModule.get("types");
		for (Object o : jsonTypes) {
			TypeDefinition td = TypeDefinition.fromJSON((JSONObject) o, result);
			td.setParent(result);
			result.addTypeDefinition(td);
		}
		JSONArray jsonDefinitions = (JSONArray) jsonModule.get("definitions");
		for (Object o : jsonDefinitions) {
			Definition d = Definition.fromJSON((JSONObject) o, result);
			d.setParent(result);
			result.addLocal(d);
		}
		for (int i = 0; i < jsonDefinitions.size(); i++)
			result.getDefinitions().get(i).updateFromJSON((JSONObject) jsonDefinitions.get(i));
		
		return result;
	}
	
	public static Module fromJSONString(String json) throws JSONException {
		JSONUtils.CheckError(json);
		return fromJSON((JSONObject) JSONValue.parse(json));
	}

	@SuppressWarnings("unchecked")
	public JSONObject toJSON()  {
		JSONObject result = new JSONObject();
		result.put("name", getName());
		JSONArray jsonImports = new JSONArray();
		result.put("imports", jsonImports);
		for (Import i: getImports())
			jsonImports.add(i.toJSON());
		JSONArray jsonTypes = new JSONArray();
		result.put("types", jsonTypes);
		for (TypeDefinition td: getTypeDefinitions())
			jsonTypes.add(td.toJSON());
		JSONArray jsonDefinitions = new JSONArray();
		result.put("definitions", jsonDefinitions);
		for (Definition d: getDefinitions())
			jsonDefinitions.add(d.toJSON());
		return result;
	}
	
	public String toJSONString() {
		StringWriter out = new StringWriter();
		try {
			toJSON().writeJSONString(out);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return out.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((definitions == null) ? 0 : definitions.hashCode());
		result = prime * result + ((imports == null) ? 0 : imports.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((types == null) ? 0 : types.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Module))
			return false;
		Module other = (Module) obj;
		if (definitions == null) {
			if (other.definitions != null)
				return false;
		} else if (!definitions.equals(other.definitions))
			return false;
		if (imports == null) {
			if (other.imports != null)
				return false;
		} else if (!imports.equals(other.imports))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (types == null) {
			if (other.types != null)
				return false;
		} else if (!types.equals(other.types))
			return false;
		return true;
	}
}
