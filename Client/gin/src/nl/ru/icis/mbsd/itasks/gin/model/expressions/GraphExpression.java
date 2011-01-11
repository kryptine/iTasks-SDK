package nl.ru.icis.mbsd.itasks.gin.model.expressions;

import java.util.ArrayList;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Edge;
import nl.ru.icis.mbsd.itasks.gin.model.Node;
import nl.ru.icis.mbsd.itasks.gin.model.Scope;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class GraphExpression extends Expression {
	private ArrayList<Node> nodes;
	private ArrayList<Edge> edges;
	private double width;
	private double height;

	public GraphExpression() {
		nodes = new ArrayList<Node>();
		edges = new ArrayList<Edge>();
		height = 50;
		width = 30;
	}
	
	public void addEdge(Edge edge) {
		edges.add(edge);
		edge.setParent(this);
		setChanged();
	}

	public void addNode(Node node) {
		nodes.add(node);
		node.setParent(this);
		setChanged();
	}

	public ArrayList<Edge> getEdges() {
		return edges;
	}

	public double getHeight() {
		return height;
	}
	
	public int getNodeIndex(Node node) {
		return nodes.indexOf(node);
	}

	public ArrayList<Node> getNodes() {
		return nodes;
	}

	public double getWidth() {
		return width;
	}
	
	public boolean hasSize() {
		return width > 0 && height > 0;
	}
	
	public void removeEdge(Edge edge) {
		edge.setParent(null);
		edges.remove(edge);
		setChanged();
	}
	
	public void removeNode(Node node) {
		node.setParent(null);
		nodes.remove(node);
		//remove all edges attached to this node
		int i = 0;
		while (i < edges.size()) {
			Edge edge = edges.get(i);
			if (edge.getFromNode() == node || edge.getToNode() == node)
				edges.remove(i);
			else
				i++;
		}
		setChanged();
	}

	public void setHeight(double height) {
		this.height = height;
		setChanged();
	}
	
	public void setWidth(double width) {
		this.width = width;
		setChanged();
	}

	public static GraphExpression fromJSON(JSONObject jsonGraphExpression, Scope scope) throws JSONException {
		GraphExpression result = new GraphExpression();
		JSONArray jsonNodes = (JSONArray) jsonGraphExpression.get("nodes");
		for (Object o : jsonNodes) {
			Node node = Node.fromJSON((JSONObject) o, scope);
			node.setParent(result);
			result.getNodes().add(node);
		}
		JSONArray jsonEdges = (JSONArray) jsonGraphExpression.get("edges");
		for (Object o : jsonEdges) {
			Edge edge = Edge.fromJSON((JSONObject) o, result.getNodes());
			edge.setParent(result);
			result.getEdges().add(edge);
		}
		if (jsonGraphExpression.get("size") != null) {
			JSONObject jsonSize = (JSONObject) jsonGraphExpression.get("size");
			result.setWidth(((Number) jsonSize.get("width")).doubleValue());
			result.setHeight(((Number) jsonSize.get("height")).doubleValue());
		}
		else {
			result.setWidth(0);
			result.setHeight(0);
		}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public JSONArray toJSON() {
		JSONArray result = new JSONArray();
		result.add("GGraphExpression");
		JSONObject jsonGraphExpression = new JSONObject();
		result.add(jsonGraphExpression);
		JSONArray jsonNodes = new JSONArray();
		jsonGraphExpression.put("nodes", jsonNodes);
		for (Node node : getNodes())
			jsonNodes.add(node.toJSON());
		JSONArray jsonEdges = new JSONArray();
		jsonGraphExpression.put("edges", jsonEdges);
		for (Edge edge : getEdges())
			jsonEdges.add(edge.toJSON(this));
		if (getWidth() > 0 && getHeight() > 0) {
			JSONObject jsonSize = new JSONObject();
			jsonSize.put("width", getWidth());
			jsonSize.put("height", getHeight());
			jsonGraphExpression.put("size", jsonSize);
		}
		else
			jsonGraphExpression.put("size", null);
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = (edges == null) ? 0 : edges.hashCode();
		long temp;
		temp = Double.doubleToLongBits(height);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + ((nodes == null) ? 0 : nodes.hashCode());
		temp = Double.doubleToLongBits(width);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof GraphExpression))
			return false;
		GraphExpression other = (GraphExpression) obj;
		if (edges == null) {
			if (other.edges != null)
				return false;
		} else if (!edges.equals(other.edges))
			return false;
		if (Double.doubleToLongBits(height) != Double
				.doubleToLongBits(other.height))
			return false;
		if (nodes == null) {
			if (other.nodes != null)
				return false;
		} else if (!nodes.equals(other.nodes))
			return false;
		if (Double.doubleToLongBits(width) != Double
				.doubleToLongBits(other.width))
			return false;
		return true;
	}
}
