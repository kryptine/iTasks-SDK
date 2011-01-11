package nl.ru.icis.mbsd.itasks.gin.model;

import java.util.ArrayList;
import java.util.Map;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeDefinition;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeExpressionContainer;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class Declaration extends Model implements Scope {
	protected Scope parentScope;
	private String name;
	private ArrayList<FormalParameter> formalParameters;
	private TypeExpressionContainer type;
	private String icon;
	private String shape;
	
	public Declaration(Scope parentScope) {
		this.parentScope = parentScope;
		setName("unnamed");
		setType(new TypeExpressionContainer());
		formalParameters = new ArrayList<FormalParameter>();
		setIcon("task");
		setShape("app:task");
	}
	
	public TypeExpressionContainer getType() {
		return type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
		setChanged();
	}
	
	public ArrayList<FormalParameter> getFormalParameters() {
		return formalParameters;
	}

	public String getIcon() {
		return icon;
	}
	
	public void setIcon(String icon) {
		this.icon = icon;
		setChanged();
	}

	public String getShape() {
		return shape;
	}

	public void setShape(String shape) {
		this.shape = shape;
		setChanged();
	}
		
	public void addFormalParameter(FormalParameter parameter) {
		formalParameters.add(parameter);
		parameter.setParent(this);
		setChanged();
	}
	
	public void addFormalParameter(int index, FormalParameter parameter) {
		formalParameters.add(index, parameter);
		parameter.setParent(this);
		setChanged();
	}

	public void removeFormalParameter(FormalParameter parameter) {
		parameter.setParent(null);
		formalParameters.remove(parameter);
		setChanged();
	}
	
	public void setType(TypeExpressionContainer type) {
		if (this.type != null)
			this.type.setParent(null);
		this.type = type;
		type.setParent(this);
		setChanged();
	}
	
	@Override
	public Map<String, Declaration> getScope() {
		return parentScope.getScope();
	}

	@Override
	public Map<String, TypeDefinition> getTypeScope() {
		return parentScope.getTypeScope();
	}
	
	@Override
	public String toString() {
		return name;
	}
		
	protected void setFromJSON(JSONObject jsonDeclaration) throws JSONException {
		setName((String) jsonDeclaration.get("name"));
		setType(TypeExpressionContainer.fromJSON(jsonDeclaration.get("returnType"), parentScope));

		JSONArray jsonFormalParameters = (JSONArray) jsonDeclaration.get("formalParams");
		for (Object o : jsonFormalParameters) {
			FormalParameter fp = FormalParameter.fromJSON((JSONObject) o, parentScope); 
			fp.setParent(this);
			addFormalParameter(fp);
		}
		
		setIcon((String) jsonDeclaration.get("icon"));		
		setShape((String) jsonDeclaration.get("shape"));
	}
	
	public static Declaration fromJSON(JSONObject jsonDeclaration, Scope scope) throws JSONException {
		Declaration result = new Declaration(scope);
		result.setFromJSON(jsonDeclaration);
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		result.put("name", getName());
		result.put("returnType", getType().toJSON());
		JSONArray jsonFormalParameters = new JSONArray();
		result.put("formalParams", jsonFormalParameters);
		for (FormalParameter parameter : getFormalParameters())
			jsonFormalParameters.add(parameter.toJSON());
		result.put("icon", getIcon());
		result.put("shape", getShape());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((formalParameters == null) ? 0 : formalParameters.hashCode());
		result = prime * result + ((icon == null) ? 0 : icon.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((shape == null) ? 0 : shape.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Declaration))
			return false;
		Declaration other = (Declaration) obj;
		if (formalParameters == null) {
			if (other.formalParameters != null)
				return false;
		} else if (!formalParameters.equals(other.formalParameters))
			return false;
		if (icon == null) {
			if (other.icon != null)
				return false;
		} else if (!icon.equals(other.icon))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (shape == null) {
			if (other.shape != null)
				return false;
		} else if (!shape.equals(other.shape))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}
}
