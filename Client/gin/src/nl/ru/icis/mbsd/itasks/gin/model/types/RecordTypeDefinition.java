package nl.ru.icis.mbsd.itasks.gin.model.types;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class RecordTypeDefinition extends TypeDefinition {

	private ArrayList<RecordField> recordFields = new ArrayList<RecordField>();

	public ArrayList<RecordField> getFields() {
		return recordFields;
	}

	public void addField(RecordField recordField) {
		recordField.setParent(this);
		recordFields.add(recordField);
		setChanged();
	}

	public static RecordTypeDefinition fromJSON(JSONArray jsonFields, Scope scope) throws JSONException {
		RecordTypeDefinition result = new RecordTypeDefinition();
		for (Object o: jsonFields)
			result.addField(RecordField.fromJSON((JSONObject)o, scope));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = super.toJSON();
		JSONArray jsonRecord = new JSONArray();
		JSONArray jsonFields = new JSONArray();
		for (RecordField r: getFields()) 
			jsonFields.add(r.toJSON());
		jsonRecord.add("GRecordTypeRhs");
		jsonRecord.add(jsonFields);
		result.put("rhs", jsonRecord);
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((recordFields == null) ? 0 : recordFields.hashCode());
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
		RecordTypeDefinition other = (RecordTypeDefinition) obj;
		if (recordFields == null) {
			if (other.recordFields != null)
				return false;
		} else if (!recordFields.equals(other.recordFields))
			return false;
		return true;
	}
}
