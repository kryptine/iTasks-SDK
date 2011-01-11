package nl.ru.icis.mbsd.itasks.gin.model.types;

public class CleanTypeExpression extends TypeExpression {
	
	private String cleanType;

	public CleanTypeExpression(String cleanType) {
		this.cleanType = cleanType; 
	}
	
	@Override
	public Object toJSON() {
		return cleanType;
	}

	@Override
	public String toString() {
		return cleanType;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((cleanType == null) ? 0 : cleanType.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof CleanTypeExpression))
			return false;
		CleanTypeExpression other = (CleanTypeExpression) obj;
		if (cleanType == null) {
			if (other.cleanType != null)
				return false;
		} else if (!cleanType.equals(other.cleanType))
			return false;
		return true;
	}

}
