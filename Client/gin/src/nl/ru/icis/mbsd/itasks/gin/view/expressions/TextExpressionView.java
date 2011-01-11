package nl.ru.icis.mbsd.itasks.gin.view.expressions;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import nl.ru.icis.mbsd.itasks.gin.graphics.PFilterSwing;
import nl.ru.icis.mbsd.itasks.gin.model.expressions.CleanExpression;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PBounds;

public class TextExpressionView extends ExpressionView implements DocumentListener, FocusListener {
	private static final long serialVersionUID = 4836187712276327208L;
	
	private JTextPane textPane;
	private PFilterSwing textNode;
	private PPath rectangle;
	
	public TextExpressionView(final CleanExpression textExpression) {
		super(textExpression);
		addDefaultDecorations();
		
		textPane = new JTextPane();
		textPane.setFont(new Font("Courier", Font.PLAIN, 12));
		textPane.setOpaque(false);
		textPane.getDocument().addDocumentListener(this);
		textPane.addFocusListener(this);
		
		textNode = new PFilterSwing(textPane);
		
		rectangle = new PPath();
		rectangle.setPathToRectangle(1,1,1,1);
		rectangle.setPaint(new Color(250, 250, 205));
		rectangle.addChild(textNode);
		addChild(rectangle);
				
		updateView(null);
	}
		
	private void resizeTextPane() {
		//Calculate line count and max line length
		char text[] = textPane.getText().toCharArray();
		int length = text.length;
		int currentLine = 0;
		int maxLine = 0;
		int lines = 1;
		for (int i = 0; i < length; i++) {
			if (text[i] == '\n') {
				lines++;
				maxLine = Math.max(maxLine, currentLine);
				currentLine = 0;
			}
			else if (text[i] == '\t') {
				currentLine += 8; 
			}
			else
				currentLine++;
		}
		maxLine = Math.max(maxLine, currentLine);
		
		//Resize textPane to fit all text without breaks
		int width = 7 * maxLine + 10;
		int height = 17 * lines + 8;
		textPane.setPreferredSize(new Dimension(width, height));
		PBounds b = new PBounds(0, 0, width, height);
		b.inset(-2, -2);
		rectangle.setBounds(b);
		setBounds(getUnionOfChildrenBounds(null));
	}

	@Override
	public void updateView(Object source) {
		CleanExpression expression = (CleanExpression) getExpression();
		textPane.setText(expression.getText());
		resizeTextPane();
	}

	@Override
	public void changedUpdate(DocumentEvent e) {
		CleanExpression expression = (CleanExpression) getExpression();
		expression.setText(textPane.getText());
		resizeTextPane();
	}

	@Override
	public void insertUpdate(DocumentEvent e) {
		changedUpdate(e);
	}

	@Override
	public void removeUpdate(DocumentEvent e) {
		changedUpdate(e);
	}

	@Override
	public void focusGained(FocusEvent arg0) {
	}

	@Override
	public void focusLost(FocusEvent arg0) {
		CleanExpression expression = (CleanExpression) getExpression();
		expression.notifyViews();
	}
}
