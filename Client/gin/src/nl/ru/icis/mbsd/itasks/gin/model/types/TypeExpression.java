package nl.ru.icis.mbsd.itasks.gin.model.types;

import nl.ru.icis.mbsd.itasks.gin.model.Model;

public abstract class TypeExpression extends Model{
	private static final long serialVersionUID = -3359227465194447842L;

	public abstract Object toJSON();
}
