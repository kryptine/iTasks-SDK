package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONObject;

public class TypeDefinition extends Model {
		private String name;
		private TypeExpressionContainer typeExpressionContainer;

		public TypeDefinition()
		{
			setName("");
			setExpressionContainer(new TypeExpressionContainer());
		}

		public String toString() {
			return name;// + " :: " + type.toString(); 
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
			setChanged();
		}

		public TypeExpressionContainer getExpressionContainer() {
			return typeExpressionContainer;
		}

		private void setExpressionContainer(TypeExpressionContainer typeExpressionContainer) {
			this.typeExpressionContainer = typeExpressionContainer; 
			setChanged();
		}
		
		public static TypeDefinition fromJSON(JSONObject jsonTypeDefinition, Scope scope) throws JSONException {
			TypeDefinition result = new TypeDefinition();
			result.setName((String) jsonTypeDefinition.get("name"));
			result.setExpressionContainer(TypeExpressionContainer.fromJSON(jsonTypeDefinition.get("expression"), scope));
			return result;
		}
		
		@SuppressWarnings("unchecked")
		public JSONObject toJSON() {
			JSONObject result = new JSONObject();
			result.put("name", getName());
			result.put("expression", getExpressionContainer().toJSON());
			return result;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((name == null) ? 0 : name.hashCode());
			result = prime
					* result
					+ ((typeExpressionContainer == null) ? 0
							: typeExpressionContainer.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (!(obj instanceof TypeDefinition))
				return false;
			TypeDefinition other = (TypeDefinition) obj;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			if (typeExpressionContainer == null) {
				if (other.typeExpressionContainer != null)
					return false;
			} else if (!typeExpressionContainer
					.equals(other.typeExpressionContainer))
				return false;
			return true;
		}
}
