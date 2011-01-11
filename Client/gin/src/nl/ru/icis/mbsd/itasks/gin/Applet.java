package nl.ru.icis.mbsd.itasks.gin;

import javax.swing.JApplet;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import netscape.javascript.JSObject;
import nl.ru.icis.mbsd.itasks.gin.view.View;

public class Applet extends JApplet implements View {
	private static final long serialVersionUID = 8407998697257424114L;
	private String id;
	private String name;
	private Editor editor;
	
	private double time;
	
	public void init() {
		try {
			id = getParameter("id");
			name = getParameter("name");
			editor = new Editor(this);
			editor.setModule(getParameter("value"));
			
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					try {
				      	add(editor);
					} catch (Exception e) {
						e.printStackTrace();
						JOptionPane.showMessageDialog(null, e.getMessage()
								.toString());
					}
				}
			});
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(null, e.getMessage()
					.toString());
		}
	}
	
	public void setValue(String value) {
		System.out.println("Server roundtrip=" + new Double(java.lang.System.currentTimeMillis() - time).toString());
		editor.setModule(value);
	}
		
	public void setError (String error) {
		System.out.println("SetError(\"" + error + "\")");
		editor.setError(error);
	}
	
	@Override
	public void updateView(Object source) {
		JSObject window = JSObject.getWindow ( this );
		double start = java.lang.System.currentTimeMillis();
		String Arguments[ ] = {id.toString(), name.toString(), editor.getModuleJSON()};
		System.out.println("json serialize=" + new Double(java.lang.System.currentTimeMillis() - start).toString());
		window.call ( "fireTaskEvent", Arguments );		
		time = java.lang.System.currentTimeMillis();
	}
}
