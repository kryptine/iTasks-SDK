package nl.ru.icis.mbsd.itasks.gin.json;


import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

public class JSONUtils {

	public static void CheckError(String json) throws JSONException {
		JSONObject error = null;
		try {
			error = (JSONObject) JSONValue.parse(json);
		} catch (Exception e) {
			return;
		}

		if (error.size() == 2 && error.containsKey("success") && error.containsKey("error")) {
			throw new JSONException((String)error.get("error"));
		}
	}

}
