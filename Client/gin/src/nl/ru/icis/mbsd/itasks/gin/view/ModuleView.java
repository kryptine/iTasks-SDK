package nl.ru.icis.mbsd.itasks.gin.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Enumeration;

import javax.swing.Icon;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import nl.ru.icis.mbsd.itasks.gin.Editor;
import nl.ru.icis.mbsd.itasks.gin.graphics.ImageCache;
import nl.ru.icis.mbsd.itasks.gin.model.Declaration;
import nl.ru.icis.mbsd.itasks.gin.model.Definition;
import nl.ru.icis.mbsd.itasks.gin.model.DefinitionContainer;
import nl.ru.icis.mbsd.itasks.gin.model.Import;
import nl.ru.icis.mbsd.itasks.gin.model.Model;
import nl.ru.icis.mbsd.itasks.gin.model.Module;

public class ModuleView extends JPanel implements View {
	private static final long serialVersionUID = 8237973233411944184L;
	private Editor editor;
	private Module module;
	private DefaultMutableTreeNode top;
	private DefaultTreeModel treeModel;
	private JTree tree;
	private JScrollPane scrollPane;
	
	public ModuleView(Editor editor) {
		super();
		this.editor = editor;
		
		setLayout(new BorderLayout());
		
		top = new DefaultMutableTreeNode();
		treeModel = new DefaultTreeModel(top);
		tree = new JTree(treeModel);
		tree.setCellRenderer(new IconRenderer());
		tree.setToggleClickCount(0); //No expand/collapse on double click
		tree.setDragEnabled(true);
		scrollPane = new JScrollPane(tree);
		add(scrollPane);
		
		tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					Declaration selection = getSelection();
					if (selection != null && selection instanceof Definition)
						ModuleView.this.editor.openDefinition((Definition)selection);
					e.consume();
				}
			}
		});
		
		tree.addKeyListener(new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				//Delete definition if backspace or delete is pressed
				if (e.getKeyChar() == 8 || e.getKeyChar() == 127) {
					Declaration selection = getSelection();
					if (selection != null && selection instanceof Definition) {
						Definition definition = (Definition)getSelection();
						if (JOptionPane.showConfirmDialog(ModuleView.this, 
								"Are you sure you want to delete \"" + selection.getName() + "\"?",
								"Delete definition",
								JOptionPane.OK_CANCEL_OPTION) == JOptionPane.OK_OPTION) {
							DefinitionContainer parent = (DefinitionContainer)definition.getParent();
							if (parent != null) {
								parent.removeLocal(definition);
								((Model)parent).notifyViews();
							}
						}
					}
				}
			}

			@Override
			public void keyPressed(KeyEvent arg0) {
			}

			@Override
			public void keyReleased(KeyEvent arg0) {
			}
		});
	}
	
	public Module getModule() {
		return module;
	}

	public void setModule(Module module) {
		this.module = module;
		module.addDeepView(this);
		update();
	}
	
	public Declaration getSelection() {
		TreePath[] paths = tree.getSelectionPaths();
		
		if (paths != null && paths.length > 0) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)paths[0].getLastPathComponent();
		if (node.getUserObject() instanceof Declaration)
			return (Declaration) node.getUserObject();
		else
			return null;
		}
		else 
			return null;
	}
		
	@SuppressWarnings("rawtypes")
	public void setSelection(Declaration definition) {
		DefaultMutableTreeNode node = null;
	    Enumeration e = top.breadthFirstEnumeration();
	    while (e.hasMoreElements()) {
	      node = (DefaultMutableTreeNode) e.nextElement();
	      if (node.getUserObject() == definition)
	        break;
	    }
	    
	    if (node.getUserObject() != definition) //not found
	    	return;
	    
        TreePath path = new TreePath(treeModel.getPathToRoot(node));
        tree.scrollPathToVisible(path);
        tree.setSelectionPath(path);
	}
	
	public void addDefinition(Definition from, DefaultMutableTreeNode to) {
		for (Definition d: from.getLocals()) {
			DefaultMutableTreeNode node = new DefaultMutableTreeNode(d);
			treeModel.insertNodeInto(node, to, to.getChildCount());
			addDefinition(d, node);
		}
	}
	
	public void addModule(Module from, DefaultMutableTreeNode to) {
		to.setUserObject(from.getName());
		treeModel.nodeChanged(to);
		
		for (Definition d: from.getDefinitions()) {
			DefaultMutableTreeNode node = new DefaultMutableTreeNode(d);
			addDefinition(d, node);
			treeModel.insertNodeInto(node, to, to.getChildCount());
		}
		
		if (! from.getImports().isEmpty()) {
			DefaultMutableTreeNode imports = new DefaultMutableTreeNode("imports");
			treeModel.insertNodeInto(imports, to, to.getChildCount());
			for (Import imp: from.getImports()) {
				DefaultMutableTreeNode impNode = new DefaultMutableTreeNode(imp);
				treeModel.insertNodeInto(impNode, imports, imports.getChildCount());
				impNode.setUserObject(imp.getName());
				for (Declaration decl: imp.getDeclarations())  {
					DefaultMutableTreeNode declNode = new DefaultMutableTreeNode(decl);
					treeModel.insertNodeInto(declNode, impNode, impNode.getChildCount());
				}
			}
		}
	}
	
	public void update()
	{
		Declaration oldSelection = getSelection();
		
		//Rebuild node tree
		while (top.getChildCount() > 0) 
			treeModel.removeNodeFromParent((DefaultMutableTreeNode)top.getChildAt(0));
		addModule(module, top);
		tree.expandPath(new TreePath(top));
		revalidate();
		setSelection(oldSelection);
	}
	
	@Override
	public void updateView(Object source) {
		update();
	}
	
	class IconRenderer extends DefaultTreeCellRenderer {
		private static final long serialVersionUID = 5565323060862902280L;
		
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean sel, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			super.getTreeCellRendererComponent(tree, value, sel, expanded,
					leaf, row, hasFocus);
			setIcon(getIcon(((DefaultMutableTreeNode)value).getUserObject()));
			return this;
		}
		
		private Icon getIcon(Object object) {
			if (object instanceof Declaration) {
				return ImageCache.getInstance().getIconFromName(((Declaration)object).getIcon());
			}
			else if (object instanceof Module) {
				return ImageCache.getInstance().getIconFromName("module");
			}
			return null;
		}
	}
}
