package nl.ru.icis.mbsd.itasks.gin.model;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.StringTokenizer;

import nl.ru.icis.mbsd.itasks.gin.view.View;

import org.json.simple.JSONArray;
import org.json.simple.JSONValue;

public abstract class Model {
	private Model parent;
	private ArrayList<Model> children;
	private ArrayList<View> views;
	private ArrayList<View> deepViews;
	private boolean changed = false;
	private boolean inNotify = false;
	private boolean inHintUpdate = false;
	private String hintMessage;
	
	public Model() {
		parent = null;
		children = new ArrayList<Model>();
		views = new ArrayList<View>();
		deepViews = new ArrayList<View>();
		changed = false;
	}
	
	public Model getParent() {
		return parent;
	}
	
	public void setParent(Model parent) {
		if (parent != this.parent) {
			if (this.parent != null)
				this.parent.children.remove(this);
			if (parent != null)
				parent.children.add(this);
		}
		this.parent = parent;
	}
	
	public void addView(View view) {
		views.add(view);
	}
	
	public void removeView(View view) {
		views.remove(view);
	}
	
	public void addDeepView(View view) {
		deepViews.add(view);
	}
	
	public void removeDeepView(View view) {
		deepViews.remove(view);
	}
	
	
	protected synchronized void setChanged()
	{
		changed = true;
	}
	
	private void childChanged(Object source) {
		for (View deepview: deepViews)
			deepview.updateView(source);
		if (parent != null)
			parent.childChanged(source);
	}
	
	private void doNotifyViews(Object source) {
		if (inNotify)
			return;
		if (! changed)
			return;
		changed = false;

		inNotify = true;
		for (View view: views)
			view.updateView(source);
		for (View view: deepViews)
				view.updateView(source);
		if (parent != null)
			parent.childChanged(source);
		inNotify = false;
	}

	public void notifyViews () {
		notifyViews(null);
	}

	public void notifyViews(Object source) {
		doNotifyViews(source);
	}
	
	public String getHintMessage() {
		return hintMessage;
	}
	
	public void setHintMessage(Object source, String hintMessage) {
		if (this.hintMessage == null && hintMessage == null
				|| this.hintMessage != null && hintMessage != null 
				&& this.hintMessage.equals(hintMessage))
			return;
		
		this.hintMessage = hintMessage;
		setChanged();
		notifyViews(source);
	}
	
	public void setHintFromJSON(Object source, String json) {
		clearHints(source);
		
		if (json.isEmpty())
			return;
		
		JSONArray jsonHints = (JSONArray) JSONValue.parse(json);
		for (Object o: jsonHints) {
			JSONArray jsonHint = (JSONArray) o;
			setHintFromPath(source, (String) jsonHint.get(0), (String) jsonHint.get(1));
		}
	}
	
	public void setHintFromPath(Object source, String path, String message) {
		System.out.println("path=" + path + ",message=" + message);
		
		StringTokenizer st = new StringTokenizer(path, "/[]", true);
		try {
			if (! st.nextToken().equals("/"))
				throw new RuntimeException("Invalid path: Expected \"/\"");
			setHintFromPath(source, st, message);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	protected void setHintFromPath(Object source, StringTokenizer st, String message) throws SecurityException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		if (! st.hasMoreTokens()) {
			setHintMessage(source, message);
		}
		else {
				String field = st.nextToken();
				if (! st.hasMoreTokens()) {
					throw new RuntimeException("Invalid path: Expected delimiter");
				}
				String delim = st.nextToken();
				if (delim.equals("[")) {
					int index = Integer.parseInt(st.nextToken());
					if (! st.nextToken().equals("]"))
						throw new RuntimeException("Invalid path: Expected \"]\"");
					if (! st.nextToken().equals("/"))
						throw new RuntimeException("Invalid path: Expected \"/\"");
					Object array = findGetter(field).invoke(this);
					Method arrayGet = array.getClass().getMethod("get", int.class);
					((Model) arrayGet.invoke(array, index)).setHintFromPath(source, st, message);
				}
				else if (delim.equals("/")){
					((Model)findGetter(field).invoke(this)).setHintFromPath(source, st, message);
				}
				else 
					throw new RuntimeException ("Invalid path: Invalid delimiter \"" + delim + "\"");
		}
	}
	
	private Method findGetter(String field) {
		String methodName = "get" + Character.toUpperCase(field.charAt(0)) + field.substring(1); 
		for (Method method : getClass().getMethods()) {
			if (method.getName().equals(methodName))
				return method;
		}
		throw new RuntimeException("getter method not found: " + this.getClass().getName() + "." + methodName + "()");
	}
	
	public void clearHints(Object source) {
		if (inHintUpdate)
			return;
		
		inHintUpdate = true;
		setHintMessage(source, null);
		for (Model child: children) {
			child.clearHints(source);
		}
		inHintUpdate = false;
	}
	
	public boolean equals (Object obj) {
		throw new UnsupportedOperationException();
	}
}
