package nl.ru.icis.mbsd.itasks.gin.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JComboBox;

import nl.ru.icis.mbsd.itasks.gin.model.Scope;
import nl.ru.icis.mbsd.itasks.gin.model.types.ApplicationTypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.ConstructorTypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeDefinition;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeExpression;
import nl.ru.icis.mbsd.itasks.gin.model.types.TypeExpressionContainer;

public class TypeExpressionContainerView extends JComboBox implements View, ActionListener {
	private static final long serialVersionUID = -983442856057955865L;
	private TypeExpressionContainer typeExpressionContainer;
	private Scope scope;
	private static String undefinedTypeExpression = "(Undefined)";

	public TypeExpressionContainerView(TypeExpressionContainer typeExpressionContainer, Scope scope) {
		super();
		this.typeExpressionContainer = typeExpressionContainer;
		this.scope = scope;
		addActionListener(this);
	}
	
	private ArrayList<TypeExpression> getTypeExpressions() {
		ArrayList<TypeExpression> result = new ArrayList<TypeExpression>();
		for (TypeDefinition td: scope.getTypeScope().values()) { 
			ConstructorTypeExpression cte = new ConstructorTypeExpression();
			cte.setTypeDefinition(td);
			result.add(cte);
		}
		return result;
	}
	
	private ArrayList<TypeExpression> getTaskTypeExpressions() {
		ArrayList<TypeExpression> result = new ArrayList<TypeExpression>();
		for (TypeExpression te: getTypeExpressions()) {
			ApplicationTypeExpression app = new ApplicationTypeExpression();
			app.setA(new TypeExpressionContainer(new ConstructorTypeExpression (scope.getTypeScope().get("Task"))));
			app.setB(new TypeExpressionContainer(te));
			result.add(app);
		}
		return result;
	}
	
	private ArrayList<TypeExpression> getAllTypeExpressions() {
		ArrayList<TypeExpression> result = new ArrayList<TypeExpression>();
		result.addAll(getTypeExpressions());
		result.addAll(getTaskTypeExpressions());
		return result;
	}

	@Override
	public void updateView(Object source) {
		if (source == this)
			return;

		removeActionListener(this);
		removeAllItems();
		addItem(undefinedTypeExpression);
		if (typeExpressionContainer.getTypeExpression() == null) {
			System.out.println("Undefined selected");
			setSelectedItem(undefinedTypeExpression);
		}
		for (TypeExpression exp: getAllTypeExpressions()) {
			addItem(exp);
			TypeExpression selected = typeExpressionContainer.getTypeExpression();
			if (selected != null && selected.toString().equals(exp.toString()))
				setSelectedItem(exp);
		}
		addActionListener(this);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		System.out.println("ActionPerformed");
		Object selectedItem = getSelectedItem();
		typeExpressionContainer.setTypeExpression(selectedItem == undefinedTypeExpression ? null : (TypeExpression)selectedItem);
		typeExpressionContainer.notifyViews(this);
	}
}
