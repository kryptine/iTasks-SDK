package nl.ru.icis.mbsd.itasks.gin.model.types;

import org.json.simple.JSONObject;

public class AbstractTypeDefinition extends TypeDefinition {

	public AbstractTypeDefinition () {
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = super.toJSON();
		result.put("rhs", "GAbstractTypeRhs");
		return result;
	}
}
