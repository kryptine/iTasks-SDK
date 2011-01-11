package nl.ru.icis.mbsd.itasks.gin.model;

import java.util.Map;

import nl.ru.icis.mbsd.itasks.gin.model.types.TypeDefinition;

public interface Scope {
	public Map<String, Declaration> getScope();
	public Map<String, TypeDefinition> getTypeScope();
}
