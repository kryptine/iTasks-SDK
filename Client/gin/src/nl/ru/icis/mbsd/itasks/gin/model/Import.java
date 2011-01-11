package nl.ru.icis.mbsd.itasks.gin.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeDefinition;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class Import extends Model implements Scope{
	private String name;
	private ArrayList<TypeDefinition> types;
	private ArrayList<Declaration> declarations;
	
	public Import() {
		this.types = new ArrayList<TypeDefinition>();
		declarations = new ArrayList<Declaration>();
	}
	
	public String getName() {
		return name;
	}
	
	private void setName(String name) {
		this.name = name;
		setChanged();
	}

	public ArrayList<TypeDefinition> getTypes() {
		return types;
	}

	public void addType(TypeDefinition definition) {
		types.add(definition);
		definition.setParent(this);
		setChanged();
	}

	public ArrayList<Declaration> getDeclarations() {
		return declarations;
	}
	
	private void addDeclaration(Declaration declaration) {
		declarations.add(declaration);
		setChanged();
	}

	@Override
	public Map<String, Declaration> getScope() {
		HashMap<String, Declaration> result = new HashMap<String, Declaration>();
		for (Declaration d : declarations)
			result.put(d.getName(), d);
		return result;
	}
	
	@Override
	public Map<String, TypeDefinition> getTypeScope() {
		HashMap<String, TypeDefinition> result = new HashMap<String, TypeDefinition>();
		for (TypeDefinition t: types)
			result.put(t.getName(), t);	
		return result;
	}
	
	public static Import fromJSON(JSONObject jsonImport) throws JSONException {
		Import result = new Import();
		result.setName((String) jsonImport.get("name"));
		JSONArray jsonTypes = (JSONArray) jsonImport.get("types");
		for (Object o : jsonTypes) {
			TypeDefinition td = TypeDefinition.fromJSON((JSONObject) o, result);
			td.setParent(result);
			result.addType(td);
		}
		JSONArray jsonImports = (JSONArray) jsonImport.get("declarations");
		for (Object o : jsonImports)
			result.addDeclaration(Declaration.fromJSON((JSONObject)o, result));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		result.put("name", getName());
		JSONArray jsonTypes = new JSONArray();
		result.put("types", jsonTypes);
		for (TypeDefinition td: getTypes())
			jsonTypes.add(td.toJSON());
		JSONArray jsonImports = new JSONArray();
		result.put("declarations", jsonImports);
		for (Declaration d: getDeclarations())
			jsonImports.add(d.toJSON());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Import))
			return false;
		Import other = (Import) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}
