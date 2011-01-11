package nl.ru.icis.mbsd.itasks.gin.view;

import java.util.Observable;
import java.util.Observer;

public class Selection extends Observable implements Observer {
	private static Selection instance;
	private Selectable selection;
	
	private Selection() {
	}
	
	public static Selection getInstance() {
		if (instance == null)
			instance = new Selection();
		return instance;
	}
	
	public Selectable get() {
		return selection;
	}
	
	public void set(Selectable newSelection) {
		if (selection == newSelection)
			return;

		Selectable oldSelection = selection;
		selection = newSelection;
		
		if (oldSelection != null)
			oldSelection.setSelected(false);
		if (newSelection != null)
			newSelection.setSelected(true);
		setChanged();
		notifyObservers();
	}
	
	public void removeSelection() {
		if (selection != null) 
			selection.Remove();
		selectNone();
	}
	
	public void selectNone() {
		set(null);
	}

	@Override
	public void update(Observable o, Object arg) {
		setChanged();
		notifyObservers();
	}
}

