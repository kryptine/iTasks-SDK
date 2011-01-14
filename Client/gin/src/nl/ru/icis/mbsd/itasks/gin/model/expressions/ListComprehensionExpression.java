package nl.ru.icis.mbsd.itasks.gin.model.expressions;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;
import nl.ru.icis.mbsd.itasks.gin.model.types.ListTypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.types.VariableTypeExpression;

public class ListComprehensionExpression extends Expression {
	private ExpressionContainer output; 
	private CleanExpression guard;
	private CleanExpression selector;
	private ExpressionContainer input;
	
	public ListComprehensionExpression()
	{
		setOutput(new ExpressionContainer());
		guard = new CleanExpression();
		selector = new CleanExpression();
		setInput(new ExpressionContainer());
		input.setExpression(new CleanExpression());
	}

	@Override
	public void setContainer(ExpressionContainer container) {
		super.setContainer(container);
		output.setTypeExpressionContainer(((ListTypeExpression)getContainer().getTypeExpressionContainer().getTypeExpression()).getElementType());
	}

	public ExpressionContainer getOutput() {
		return output;
	}

	private void setOutput(ExpressionContainer output) {
		if (this.output != null)
			this.output.setParent(null);
		this.output = output;
		output.setParent(this);
		setChanged();
	}

	public CleanExpression getGuard() {
		return guard;
	}

	private void setGuardText(String guard) {
		this.guard.setText(guard);
	}
	
	public CleanExpression getSelector() {
		return selector;
	}
	
	private void setSelectorText (String selector) {
		this.selector.setText(selector);
	}

	public ExpressionContainer getInput() {
		return input;
	}

	private void setInput(ExpressionContainer input) {
		if (this.input != null)
			this.input.setParent(null);
		this.input = input;
		input.setParent(this);
		
		//Input has type List[a]
		ListTypeExpression inputType = new ListTypeExpression();
		inputType.setElementType(new TypeExpressionContainer(new VariableTypeExpression("a")));
		input.setTypeExpressionContainer(new TypeExpressionContainer(inputType));
		setChanged();
	}
	
	public static ListComprehensionExpression fromJSON(JSONObject jsonObject, ListTypeExpression type, Scope scope) throws JSONException {
		ListComprehensionExpression result = new ListComprehensionExpression();
		result.setOutput(ExpressionContainer.fromJSON(jsonObject.get("output"), type.getElementType(), scope));
		result.setGuardText((String) jsonObject.get("guard"));
		result.setSelectorText((String) jsonObject.get("selector"));
		result.setInput(ExpressionContainer.fromJSON(jsonObject.get("input"), new TypeExpressionContainer(), scope));
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public JSONArray toJSON() {
		JSONArray result = new JSONArray();
		result.add("GListComprehensionExpression");		
		JSONObject jsonListComp = new JSONObject();
		result.add(jsonListComp);
		
		jsonListComp.put("output", getOutput().toJSON());
		jsonListComp.put("guard", getGuard().getText());
		jsonListComp.put("selector", getSelector().getText());
		jsonListComp.put("input", getInput().toJSON());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = (guard == null) ? 0 : guard.hashCode();
		result = prime * result + ((input == null) ? 0 : input.hashCode());
		result = prime * result + ((output == null) ? 0 : output.hashCode());
		result = prime * result
				+ ((selector == null) ? 0 : selector.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof ListComprehensionExpression))
			return false;
		ListComprehensionExpression other = (ListComprehensionExpression) obj;
		if (guard == null) {
			if (other.guard != null)
				return false;
		} else if (!guard.equals(other.guard))
			return false;
		if (input == null) {
			if (other.input != null)
				return false;
		} else if (!input.equals(other.input))
			return false;
		if (output == null) {
			if (other.output != null)
				return false;
		} else if (!output.equals(other.output))
			return false;
		if (selector == null) {
			if (other.selector != null)
				return false;
		} else if (!selector.equals(other.selector))
			return false;
		return true;
	}
}
