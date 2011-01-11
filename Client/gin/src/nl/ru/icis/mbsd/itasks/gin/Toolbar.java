package nl.ru.icis.mbsd.itasks.gin;

import java.awt.SystemColor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JToolBar;

import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.model.Declaration;
import nl.ru.icis.mbsd.itasks.gin.model.Definition;
import nl.ru.icis.mbsd.itasks.gin.model.Edge;
import nl.ru.icis.mbsd.itasks.gin.model.ExpressionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.CleanExpression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.Expression;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.GraphExpression;
import nl.ru.icis.mbsd.itasks.gin.view.EdgeView;
import nl.ru.icis.mbsd.itasks.gin.view.Selectable;
import nl.ru.icis.mbsd.itasks.gin.view.Selection;

public class Toolbar extends JToolBar implements Observer{
	private static final long serialVersionUID = 6869538679105151660L;
	
	private static Toolbar instance;
	private Editor editor;
	private JButton togglePatternButton;
	private JButton removeButton;
	
	public static Toolbar getInstance() {
		return instance;
	}
	
	public Toolbar(final Editor editor) {
		instance = this;
		this.editor = editor;
		setFloatable(false);
		
		Selection.getInstance().addObserver(this);
		
		drawingModeButtons = new ArrayList<JButton>();
		addDrawingModeButton("Arrow", "cursor22", DrawingMode.ARROW);
		addDrawingModeButton("Connector", "connector22", DrawingMode.CONNECTOR);

		addSeparator();
		togglePatternButton = addButton("Pattern", "pattern22", new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				Selectable selection = Selection.getInstance().get();
				if (selection instanceof EdgeView) {
					Edge edge = ((EdgeView)selection).getEdge(); 
					if (edge.getPattern() != null)
						edge.setPattern(null);
					else
						edge.setPattern(new CleanExpression());
					edge.notifyViews();
				}
			}
		});
		togglePatternButton.setEnabled(false);
		
		addButton("Add subworkflow", "task22", new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				GraphExpression graphExpression = new GraphExpression(); 
				graphExpression.setWidth(0); 
				graphExpression.setHeight(0);
				addLocal(graphExpression, "task", "app:task");
			}
		});
		addButton("Add function", "function22", new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				addLocal(new CleanExpression(), "function", "app:function");
			}
		});
		removeButton = addButton("Remove selection", "remove22", new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Selection.getInstance().removeSelection();
			}
		});
			
		addButton("Debug:SetJSON", "bug", new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String json = JOptionPane.showInputDialog("Edit JSON", editor.getModuleJSON());
				if (json != null) {
					editor.setModule(json);
				}
			}
		});
		/*
		addButton("Debug:SetError", "bug", new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String err = JOptionPane.showInputDialog("SetError", "[[\"/definitions[0]/body/nodes[0]/actualParams[0]/\", \"SomeError\"]]");
				if (err != null) {
					editor.setError(err);
				}
			}
		});*/
	}
	
	protected void addLocal(Expression expression, String icon, String shape) {
		Declaration selectedDeclaration = editor.getModuleView().getSelection();
		if (selectedDeclaration != null && selectedDeclaration instanceof Definition) {
			Definition parent = (Definition)selectedDeclaration;
			Definition local = new Definition(parent);
			ExpressionContainer exc = local.getBody();
			exc.setTypeExpressionContainer(local.getType());
			exc.setExpression(expression);
			parent.addLocal(local);
			local.setIcon(icon);
			local.setShape(shape);
			editor.openDefinition(local);
			selectedDeclaration.notifyViews();
		}
	}
	
	protected JButton addButton(String name, String iconName, ActionListener al) {
		JButton button = new JButton();
		button.setIcon(ImageCache.getInstance().getIconFromName(iconName));
		button.setToolTipText(name);
		button.addActionListener(al);
		add(button);
		return button;
	}
	
	/* 
	 * Drawing modes 
	 */
	private DrawingMode drawingMode;
	private ArrayList<JButton> drawingModeButtons;

	public enum DrawingMode {
		ARROW, CONNECTOR };
	
	protected void addDrawingModeButton(String name, String toolTipText, final DrawingMode drawingMode)
	{
		JButton button = addButton(name, toolTipText, new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				for (JButton button: drawingModeButtons) {
					button.setBackground(SystemColor.control);
				}
				((JButton)e.getSource()).setBackground(SystemColor.textHighlight);
				Toolbar.this.drawingMode = drawingMode;
			}
		});
		if (drawingModeButtons.isEmpty()) {
			button.setBackground(SystemColor.textHighlight);
			this.drawingMode = drawingMode; 
		}

		drawingModeButtons.add(button);
	}

	public DrawingMode getDrawingMode() {
		return drawingMode;
	}
	
	@Override
	public void update(Observable o, Object arg) {
		Selectable selection = Selection.getInstance().get();
		togglePatternButton.setEnabled(selection instanceof EdgeView);
		removeButton.setEnabled(selection != null);
	}
}
	
