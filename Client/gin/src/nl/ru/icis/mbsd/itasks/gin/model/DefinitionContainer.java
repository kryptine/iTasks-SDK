package nl.ru.icis.mbsd.itasks.gin.model;

public interface DefinitionContainer extends Scope {
	public void addLocal(Definition definition);
	public void removeLocal(Definition definition);
}
