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
	private boolean inError = false;
	private String errorMessage;
	
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
	
	public String getErrorMessage() {
		return errorMessage;
	}
	
	public void setErrorMessage(Object source, String errorMessage) {
		if (this.errorMessage == null && errorMessage == null
				|| this.errorMessage != null && errorMessage != null 
				&& this.errorMessage.equals(errorMessage))
			return;
		
		this.errorMessage = errorMessage;
		setChanged();
		notifyViews(source);
	}
	
	public void setErrorFromJSON(Object source, String json) {
		clearErrors(source);
		
		if (json.isEmpty())
			return;
		
		JSONArray jsonErrors = (JSONArray) JSONValue.parse(json);
		for (Object o: jsonErrors) {
			JSONArray jsonError = (JSONArray) o;
			setErrorFromPath(source, (String) jsonError.get(0), (String) jsonError.get(1));
		}
	}
	
	public void setErrorFromPath(Object source, String path, String message) {
		System.out.println("path=" + path + ",message=" + message);
		
		StringTokenizer st = new StringTokenizer(path, "/[]", true);
		try {
			if (! st.nextToken().equals("/"))
				throw new RuntimeException("Invalid path: Expected \"/\"");
			setErrorFromPath(source, st, message);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	protected void setErrorFromPath(Object source, StringTokenizer st, String message) throws SecurityException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
		if (! st.hasMoreTokens()) {
			setErrorMessage(source, message);
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
					((Model) arrayGet.invoke(array, index)).setErrorFromPath(source, st, message);
				}
				else if (delim.equals("/")){
					((Model)findGetter(field).invoke(this)).setErrorFromPath(source, st, message);
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
	
	public void clearErrors(Object source) {
		if (inError)
			return;
		
		inError = true;
		setErrorMessage(source, null);
		for (Model child: children) {
			child.clearErrors(source);
		}
		inError = false;
	}
	
	public boolean equals (Object obj) {
		throw new UnsupportedOperationException();
	}
}
