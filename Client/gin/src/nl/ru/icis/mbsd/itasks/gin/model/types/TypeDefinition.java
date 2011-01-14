package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class TypeDefinition extends Model {
		private String name;

		public TypeDefinition()
		{
		}

		public String toString() {
			return name; 
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
			setChanged();
		}

		public static TypeDefinition fromJSON(JSONObject jsonTypeDefinition, Scope scope) throws JSONException {
			
			String name = (String) jsonTypeDefinition.get("name");
			TypeDefinition result = null;
			
			if (jsonTypeDefinition.get("rhs") instanceof JSONArray) {
				JSONArray rhs = (JSONArray) jsonTypeDefinition.get("rhs");
				if (rhs.size() != 2)
					throw new JSONException("Expected array size 2");
				String constructor = (String) rhs.get(0);
				if (constructor.equals("GAlgebraicTypeRhs"))
					result = AlgebraicTypeDefinition.fromJSON ((JSONArray) rhs.get(1), scope);
				else if (constructor.equals("GRecordTypeRhs"))
					result = RecordTypeDefinition.fromJSON ((JSONArray) rhs.get(1), scope);
				else if (constructor.equals ("GSynonymTypeRhs"))
					result = SynonymTypeDefinition.fromJSON (rhs.get(1), scope);
			} else {
				if (((String)jsonTypeDefinition.get("rhs")).equals("GAbstractTypeRhs")) {
					result = new AbstractTypeDefinition();
				}
			}
			
			if (result == null)
				throw new JSONException("TypeRhs not found:");
			
			result.setName(name);
			return result;
		}

		@SuppressWarnings("unchecked")
		public JSONObject toJSON() {
			JSONObject result = new JSONObject();
			result.put("name", getName());
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
			if (getClass() != obj.getClass())
				return false;
			TypeDefinition other = (TypeDefinition) obj;
			if (name == null) {
				if (other.name != null)
					return false;
			} else if (!name.equals(other.name))
				return false;
			return true;
		}
}
