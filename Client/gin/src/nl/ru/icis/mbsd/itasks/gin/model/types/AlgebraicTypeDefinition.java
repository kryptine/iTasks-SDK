package nl.ru.icis.mbsd.itasks.gin.model.types;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class AlgebraicTypeDefinition extends TypeDefinition {

	private ArrayList<DataConstructor> dataConstructors = new ArrayList<DataConstructor>();

	public ArrayList<DataConstructor> getDataConstructors() {
		return dataConstructors;
	}

	public void addDataConstructor(DataConstructor dataConstructor) {
		dataConstructors.add(dataConstructor);
		dataConstructor.setParent(this);
		setChanged();
	}

	public static AlgebraicTypeDefinition fromJSON(JSONArray jsonDataConstructors, Scope scope) throws JSONException {
		AlgebraicTypeDefinition result = new AlgebraicTypeDefinition();
		for (Object o: jsonDataConstructors)
			result.addDataConstructor(DataConstructor.fromJSON((JSONObject)o, scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = super.toJSON();
		JSONArray jsonAlgebraic = new JSONArray();
		JSONArray jsonDataConstructors = new JSONArray();
		for (DataConstructor r: getDataConstructors()) 
			jsonDataConstructors.add(r.toJSON());
		jsonAlgebraic.add("GAlgebraicTypeRhs");
		jsonAlgebraic.add(jsonDataConstructors);
		result.put("rhs", jsonAlgebraic);
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((dataConstructors == null) ? 0 : dataConstructors.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		AlgebraicTypeDefinition other = (AlgebraicTypeDefinition) obj;
		if (dataConstructors == null) {
			if (other.dataConstructors != null)
				return false;
		} else if (!dataConstructors.equals(other.dataConstructors))
			return false;
		return true;
	}
}
