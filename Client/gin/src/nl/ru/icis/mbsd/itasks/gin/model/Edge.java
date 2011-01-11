package nl.ru.icis.mbsd.itasks.gin.model;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.model.expressions.CleanExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;

import org.json.simple.JSONObject;

public class Edge extends Model {
	private Node fromNode;
	private Node toNode;
	private CleanExpression pattern = null;
	
	public Edge() {
	}
	
	public Node getFromNode() {
		return fromNode;
	}

	public void setFromNode(Node fromNode) {
		this.fromNode = fromNode;
		setChanged();
	}

	public Node getToNode() {
		return toNode;
	}

	public void setToNode(Node toNode) {
		setChanged();
		this.toNode = toNode;
	}

	public Edge(Node fromNode, Node toNode) {
		this.fromNode = fromNode;
		this.toNode = toNode;
	}

	public void setPattern(CleanExpression pattern) {
		if (this.pattern != null)
			this.pattern.setParent(null);
		this.pattern = pattern;
		if (pattern != null)
			pattern.setParent(this);
		setChanged();
	}

	public CleanExpression getPattern() {
		return pattern;
	}
	
	public static Edge fromJSON(JSONObject jsonEdge, ArrayList<Node>nodes) {
		Edge result = new Edge();
		int fromNode = ((Number) jsonEdge.get("fromNode")).intValue();
		int toNode = ((Number) jsonEdge.get("toNode")).intValue();
		CleanExpression pattern = null;
		if (jsonEdge.get("pattern") != null) {
			pattern = CleanExpression.fromJSON((String) jsonEdge.get("pattern"));
			pattern.setParent(result);
		}
		result.setFromNode(nodes.get(fromNode));
		result.setToNode(nodes.get(toNode));
		result.setPattern(pattern);
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public JSONObject toJSON(GraphExpression expression) {
		JSONObject result = new JSONObject();
		result.put("fromNode", expression.getNodeIndex(getFromNode()));
		result.put("toNode", expression.getNodeIndex(getToNode()));
		result.put("pattern", getPattern() == null ? null : getPattern().getText());
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((fromNode == null) ? 0 : fromNode.hashCode());
		result = prime * result + ((pattern == null) ? 0 : pattern.hashCode());
		result = prime * result + ((toNode == null) ? 0 : toNode.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Edge))
			return false;
		Edge other = (Edge) obj;
		if (fromNode == null) {
			if (other.fromNode != null)
				return false;
		} else if (!fromNode.equals(other.fromNode))
			return false;
		if (pattern == null) {
			if (other.pattern != null)
				return false;
		} else if (!pattern.equals(other.pattern))
			return false;
		if (toNode == null) {
			if (other.toNode != null)
				return false;
		} else if (!toNode.equals(other.toNode))
			return false;
		return true;
	}
}
