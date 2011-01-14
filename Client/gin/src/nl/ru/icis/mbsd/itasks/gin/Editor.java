package nl.ru.icis.mbsd.itasks.gin;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;

import nl.ru.icis.mbsd.itasks.gin.json.JSONException;
import nl.ru.icis.mbsd.itasks.gin.model.Definition;
import nl.ru.icis.mbsd.itasks.gin.model.Module;
import nl.ru.icis.mbsd.itasks.gin.view.DefinitionView;
import nl.ru.icis.mbsd.itasks.gin.view.ModuleView;
import nl.ru.icis.mbsd.itasks.gin.view.Selection;
import nl.ru.icis.mbsd.itasks.gin.view.View;

public class Editor extends JPanel {

	private static final long serialVersionUID = -764369268463469976L;

	private View parent;
	
	private ModuleView moduleView = null;
	private DefinitionView definitionView = null;
	
	private Toolbar toolbar = null;
	private JPanel centerPanel = null;
	private JLabel statusBar; 

	private Dimension minimumSize;

	private String lastHint = null;

	public Editor(View parent) {
		this.parent = parent;
		
		setLayout(new BorderLayout());

		toolbar = new Toolbar(this);
		add(toolbar, BorderLayout.NORTH);
		
		moduleView = new ModuleView(this);

		centerPanel = new JPanel();
		centerPanel.setLayout(new BorderLayout());

		statusBar = new JLabel();
		statusBar.setPreferredSize(new Dimension(1, 16));
		centerPanel.add(statusBar, BorderLayout.SOUTH);
		
		JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                moduleView, centerPanel);
		splitPane.setDividerLocation(150);
		minimumSize = new Dimension(100, 50);
		moduleView.setMinimumSize(minimumSize);
		centerPanel.setMinimumSize(minimumSize);
		add(splitPane, BorderLayout.CENTER);
	}
	
	public Module getModule() {
		return moduleView.getModule();
	}
	
	public String getModuleJSON() {
		return getModule().toJSONString();
	}
		
	public void setModule(Module module) {
		try {
			if (module.equals(getModule())) {
				System.out.println("module is equal");
				return;
			}
			System.out.println("module has changed");

			if (moduleView.getModule() != null)
				moduleView.getModule().removeDeepView(parent);
			moduleView.setModule(module);
			if (lastHint != null)
				setHint(lastHint);
			Selection.getInstance().set(null);
			openDefinition(module.getDefinitions().get(0));
			module.addDeepView(parent);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void setModule(String value) {
		try {
			setModule(Module.fromJSONString(value));
		} catch (JSONException e) {
			e.printStackTrace();
		}
	}
	
	public ModuleView getModuleView() {
		return moduleView;
	}

	public void setModuleView(ModuleView moduleView) {
		this.moduleView = moduleView;
	}
	
	public void openDefinition(Definition definition) {
		moduleView.setSelection(definition);
		if (definitionView != null)
			centerPanel.remove(definitionView);
		definitionView = new DefinitionView(definition, moduleView);
		centerPanel.add(definitionView, BorderLayout.CENTER);
		centerPanel.revalidate();
	}
	
	public void setHint(String error) {
		this.lastHint = error;
		getModule().setHintFromJSON(parent, error);
		
		if (getModule().getHintMessage() != null) 
			statusBar.setText(getModule().getHintMessage());
		else
			statusBar.setText("Ready");
	}
}
