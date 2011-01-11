package nl.ru.icis.mbsd.itasks.gin.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.dnd.DropTarget;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import nl.ru.icis.mbsd.itasks.gin.DragAndDropHandler;
import nl.ru.icis.mbsd.itasks.gin.graphics.SwingUtils;
import nl.ru.icis.mbsd.itasks.gin.model.Definition;
import edu.umd.cs.piccolox.pswing.PSwingCanvas;
import edu.umd.cs.piccolox.swing.PScrollPane;

public class DefinitionView extends JPanel implements View, FocusListener {
	private static final long serialVersionUID = 4173120394811038304L;
	private static final int CANVAS_HEIGHT = 4096;
	private static final int CANVAS_WIDTH = 4096;
	Definition definition;
	
	private JPanel headerPanel;
	private JTextField nameField;
	private TypeExpressionContainerView typeField;
	private FormalParameterListView parameterPanel;
	
	private PSwingCanvas canvas;
	
	ExpressionContainerView expressionContainerView;
	
	public DefinitionView(Definition definition, ModuleView libraryView) {
		super();
		this.definition = definition;
		definition.addView(this);
		setLayout(new BorderLayout());
				
		headerPanel = new JPanel();
		headerPanel.setLayout(new GridBagLayout());
		add(headerPanel, BorderLayout.NORTH);
		
		headerPanel.add(new JLabel("Name"), SwingUtils.makeGridBagConstraints(0, 0, 0, false));
		nameField = new JTextField();
		headerPanel.add(nameField, SwingUtils.makeGridBagConstraints(1, 0, 1.0, true));
		nameField.addFocusListener(this);
		
		headerPanel.add(new JLabel("Inputs"), SwingUtils.makeGridBagConstraints(0, 1, 0, false));
		parameterPanel = new FormalParameterListView(definition, this);
		headerPanel.add(parameterPanel, SwingUtils.makeGridBagConstraints(1, 1, 1.0, true));

		headerPanel.add(new JLabel("Output"), SwingUtils.makeGridBagConstraints(0, 2, 0, false));
		typeField = new TypeExpressionContainerView(definition.getType(), definition);
		headerPanel.add(typeField, SwingUtils.makeGridBagConstraints(1, 2, 1.0, true));
		typeField.addFocusListener(this);
		
		canvas = new PSwingCanvas();
		canvas.setPreferredSize(new Dimension(CANVAS_WIDTH, CANVAS_HEIGHT));
		canvas.removeInputEventListener(canvas.getPanEventHandler());
		canvas.removeInputEventListener(canvas.getZoomEventHandler());
		add(new PScrollPane(canvas), BorderLayout.CENTER);
	
		expressionContainerView = new ExpressionContainerView(definition.getBody());
		canvas.getLayer().addChild(expressionContainerView);
		
		new DropTarget(canvas, new DragAndDropHandler(libraryView, canvas));
		
		updateView(null);
	}
	
	public Definition getDefinition() {
		return definition;
	}

	@Override
	public void updateView(Object source) {
		if (source == this)
			return;
		
		nameField.setText(definition.getName());
		typeField.updateView(source);
	}
	
	@Override
	public void focusGained(FocusEvent e) { }

	@Override
	public void focusLost(FocusEvent arg0) {
		definition.setName(nameField.getText());
		definition.notifyViews(this);
	}

}
