package nl.ru.icis.mbsd.itasks.gin.model;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class Node extends Model {
	private Declaration declaration;
	private Point2D position;
	
	private HashMap<FormalParameter,ExpressionContainer> actualParameters;
	
	public Node(Declaration declaration) {
		this.declaration = declaration;
		position = new Point2D.Double(0.0, 0.0);
		actualParameters = new HashMap<FormalParameter, ExpressionContainer>();
		for (FormalParameter formalParameter : declaration.getFormalParameters())
			getActualParam(formalParameter);
	}
	
	public Point2D getPosition() {
		return position;
	}
	
	public void setPosition(Point2D position) {
		this.position = position;
		setChanged();
	}
	
	public Declaration getDeclaration() {
		return declaration;
	}

	public ArrayList<ExpressionContainer> getActualParams() {
		ArrayList<ExpressionContainer> result = new ArrayList<ExpressionContainer>();
		for (FormalParameter fp: getDeclaration().getFormalParameters()) {
			result.add(getActualParam(fp));
		}
		return result;
	}
	
	public ExpressionContainer getActualParam(FormalParameter formalParameter) {
		ExpressionContainer value;
		value = actualParameters.get(formalParameter);
		
		if (value == null) {
			value = new ExpressionContainer();
			value.setTypeExpressionContainer(formalParameter.getType());
			value.setParent(this);
			setActualParam(formalParameter, value);
		}
		return value;
	}
	
	public void setActualParam(FormalParameter formalParameter, ExpressionContainer value) {
		value.setParent(this);
		actualParameters.put(formalParameter, value);
		setChanged();
	}

	public void checkRemovedParams() {
		//Delete actual parameters whose corresponding formal parameter is deleted
		Iterator<Entry<FormalParameter, ExpressionContainer>> i = actualParameters.entrySet().iterator(); 
		while (i.hasNext()) {
			Map.Entry<FormalParameter, ExpressionContainer> entry = i.next();
			if (! declaration.getFormalParameters().contains(entry.getKey())) {
				entry.getValue().setParent(null);
				i.remove();
			}
		}
	}
	
	public static Node fromJSON (JSONObject jsonNode, Scope scope) throws JSONException {
		String name = (String) jsonNode.get("name");
		Declaration declaration = scope.getScope().get(name);
		if (declaration == null)
			throw new JSONException("Not in scope: " + name);
		Node result = new Node(declaration);
		JSONArray jsonActualParameters = (JSONArray) jsonNode
				.get("actualParams");
		for (int i = 0; i < declaration.getFormalParameters().size(); i++) {
			FormalParameter formalParameter = declaration.getFormalParameters().get(i);
			ExpressionContainer exc = ExpressionContainer.fromJSON(jsonActualParameters.get(i), formalParameter.getType(), scope);
			exc.setParent(result);
			result.setActualParam(formalParameter, exc); 
		}

		JSONObject positionNode = (JSONObject) jsonNode.get("position");
		double x = ((Number) positionNode.get("x")).doubleValue();
		double y = ((Number) positionNode.get("y")).doubleValue();
		result.setPosition(new Point2D.Double(x, y));
		return result;
	}

	@SuppressWarnings("unchecked")
	public JSONObject toJSON() {
		JSONObject result = new JSONObject();
		result.put("name", declaration.getName());
		JSONArray jsonActualParameters = new JSONArray();
		result.put("actualParams", jsonActualParameters);
		for (ExpressionContainer actualParameter : getActualParams())
			jsonActualParameters.add(actualParameter.toJSON());
		JSONObject positionNode = new JSONObject();
		result.put("position", positionNode);
		positionNode.put("x", (int)getPosition().getX());
		positionNode.put("y", (int)getPosition().getY());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((actualParameters == null) ? 0 : actualParameters.hashCode());
		result = prime * result
				+ ((declaration == null) ? 0 : declaration.hashCode());
		result = prime * result
				+ ((position == null) ? 0 : position.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Node))
			return false;
		Node other = (Node) obj;
		if (actualParameters == null) {
			if (other.actualParameters != null)
				return false;
		} else if (!actualParameters.equals(other.actualParameters))
			return false;
		if (declaration == null) {
			if (other.declaration != null)
				return false;
		} else if (!declaration.getName().equals(other.declaration.getName()))
			return false;
		if (position == null) {
			if (other.position != null)
				return false;
		} else if (!position.equals(other.position))
			return false;
		return true;
	}
}
