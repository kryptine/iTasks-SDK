package nl.ru.icis.mbsd.itasks.gin.view;

import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.graphics.SwingUtils;
import nl.ru.icis.mbsd.itasks.gin.model.Definition;
import nl.ru.icis.mbsd.itasks.gin.model.FormalParameter;

public class FormalParameterListView extends JPanel implements View {
	private static final long serialVersionUID = 7505758348421089041L;
	private Definition definition;
	private DefinitionView definitionView;

	public FormalParameterListView(Definition definition, DefinitionView definitionView) {
		this.definition = definition;
		this.definitionView = definitionView;
		definition.addView(this);
		setLayout(new GridBagLayout());
		updateView(null);
	}
	
	private boolean parameterExists(String name) {
		for (FormalParameter p: definition.getFormalParameters())
			if (p.getName().equals(name))
				return true;
		return false;
	}
	
	private String makeUniqueName(int index) {
		int i = index + 1;
		String name = "parameter" + i;
		while (parameterExists (name)) {
			i++;
			name = "parameter" + i;
		}
		return name;
	}
	
	private void addFormalParameter(int index) {
		FormalParameter formalParameter = new FormalParameter(definition);
		formalParameter.setName(makeUniqueName(index));
		definition.addFormalParameter(index, formalParameter);
	}

	public void updateView(Object source) {
		if (source == this || source == definitionView)
			return;
		
		assert java.awt.EventQueue.isDispatchThread();
		
		ArrayList<FormalParameter> formalParameters = definition.getFormalParameters();
		
		removeAll();
		for (int i = 0; i < formalParameters.size(); i++) {

			final int index = i;
			final FormalParameter fp = formalParameters.get(i);

			final JTextField nameField = new JTextField(fp.getName());
			add(nameField, SwingUtils.makeGridBagConstraints(0, i, 0.25, true));
			final TypeExpressionContainerView typeField = new TypeExpressionContainerView(fp.getType(), definition);
			typeField.updateView(this);
			add(typeField, SwingUtils.makeGridBagConstraints(1, i, 0.75, true));
			FocusListener listener = new FocusListener() {
				@Override
				public void focusGained(FocusEvent e) { }

				@Override
				public void focusLost(FocusEvent arg0) {
					fp.setName(nameField.getText());
					fp.notifyViews(this);
				}
			};
			nameField.addFocusListener(listener);
			typeField.addFocusListener(listener);
			final JButton addButton = new JButton();
			addButton.setIcon(ImageCache.getInstance().getIconFromName("add16"));
			addButton.setToolTipText("Add a parameter above");
			add(addButton, SwingUtils.makeGridBagConstraints(2, i, 0, false));
			addButton.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					addFormalParameter(index);
					definition.notifyViews();
				}
			});

			final JButton removeButton = new JButton();
			removeButton.setIcon(ImageCache.getInstance().getIconFromName("remove16"));
			removeButton.setToolTipText("Delete this parameter");
			add(removeButton, SwingUtils.makeGridBagConstraints(3, i, 0, false));
			removeButton.addActionListener(new ActionListener() {
				
				@Override
				public void actionPerformed(ActionEvent e) {
					definition.removeFormalParameter(fp);
					definition.notifyViews();
				}
			});
		}
		
		if (formalParameters.isEmpty()) {
			add( new JLabel("(none)"), SwingUtils.makeGridBagConstraints(0, 0, 1.0, true));
			JButton addButton = new JButton();
			addButton.setIcon(ImageCache.getInstance().getIconFromName("add16"));
			addButton.setToolTipText("Add a new parameter");
			add(addButton, SwingUtils.makeGridBagConstraints(1, 0, 0, false));
			addButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				addFormalParameter(0);
				definition.notifyViews();
			}
		});
		}
		repaint();
		//revalidate();
	}
}
