Ext.ns('itasks.ttc')

itasks.ttc.GroupContainer = Ext.extend(itasks.ttc.TTCBase,{
	initComponent: function(){
		Ext.apply(this, 
		{ layout:'auto'
		, cls: 'TTCGroupContainer'
		, autoScroll: true
		, unstyled: true
		, fixedCont: new Ext.util.MixedCollection()		// collection of all fixed containers
		, floatingCont: new Ext.util.MixedCollection()	// collection of all floating containers
		, url: itasks.config.serviceUrl + "/json/tasks/" + this.taskId + "/tui"
		});

		itasks.ttc.GroupContainer.superclass.initComponent.apply(this,arguments);
		
		for(var i=0; i < this.content.length; i++) {
			var cont = this.createContainer(this.content[i].panel,this.content[i].behaviour,this.content[i].index);
			
			if(this.content[i].behaviour == 'Floating' || this.content[i].behaviour == 'GBModal') {
				this.floatingCont.add(this.content[i].index, cont);
			} else {
				this.fixedCont.add(this.content[i].index, cont);
			}		
		}
	},
	createContainer: function(cont, behaviour, idx) {
		
		if(behaviour == 'Floating' || behaviour == 'Modal') {	
			return this.add(new itasks.ttc.GroupItemWindow({
				id: this.taskId + '_' + idx,
				index: idx,
				cls: 'TTCGroupItemWindow',
				closable: false,
				autoScroll: true,
				maximizable: true,
				constrainHeader: true,
				modal: behaviour == 'Modal',
				items: [cont],
				title: cont.subject
			}));
		} else {
			//Default is fixed items on the main canvas.
			return this.add(new Ext.Panel({
				index: idx,
				cls: 'TTCGroupItemPanel',
				items: [cont],
				unstyled: true
			}));
		}
	},
	onLayout: function() {
		itasks.ttc.GroupContainer.superclass.onLayout.call(this,arguments);
		
		//Show the floating windows after layouting the components
		this.floatingCont.each(function(cont) {
			cont.show();
		});
	},
	update: function(data) {
		var content = data.content;
		
		// During the update, keep track of two sets of containers
		// The 'old' containers that already existed and 'new' containers
		// that exist after the update
		var oldFixed = this.fixedCont;
		var newFixed = new Ext.util.MixedCollection();
		
		var oldFloating = this.floatingCont;
		var newFloating = new Ext.util.MixedCollection();
		
		var focusFloating = new Ext.util.MixedCollection();
				
		//Loop over content and update/add child itesm	
		for(var i=0; i < content.length; i++) {
		
			var cont;
			var panel = content[i].panel;
			var index = content[i].index;
			var behaviour = content[i].behaviour;
			var focus = content[i].focus;
					
			if(cont = oldFixed.key(index)) {
				// update existing fixed container
				this.updateItem(cont, panel, behaviour, index);
				
				oldFixed.remove(cont);
				newFixed.add(index, cont);
					
			} else if(cont = oldFloating.key(index)) {
				// update existing floating container
				this.updateItem(cont, panel, behaviour);
				
				oldFloating.remove(cont);
				newFloating.add(index, cont);
				
				if (focus)
					focusFloating.add(cont);
					
			} else {
				// Create a new item new container
				cont = this.addItem(panel, behaviour, index);
				
				// Add the component to this component's items			
				if(behaviour == 'Floating' || behaviour == 'Modal') {
					newFloating.add(index, cont);
					this.doLayout();
					cont.show();
				} else {
					newFixed.add(index, cont);
					this.doLayout();
				}
				
			}
		}
		// destroy containers that are left in the old sets
		oldFixed.each(function(item) {item.destroy();});
		oldFloating.each(function(item) {item.destroy();});
		
		// from now on, use the new container sets
		this.fixedCont = newFixed;
		this.floatingCont = newFloating;
	
		// focus the floating windows
		focusFloating.each(function(c) {
			c.toFront();
		}, this);
		
	},
	addItem: function(panel, behaviour, index) {
		return this.createContainer(panel, behaviour, index);
	},
	updateItem: function(container, panel, behaviour, index) {
	
		var oldPanel = container.get(0);
		if(oldPanel.getXType() == panel.xtype && oldPanel.taskId == panel.taskId) {
			// update container contents
			oldPanel.update(panel);
		} else {
			//if not same xtype or taskId - completely replace container contents
			container.removeAll();
			container.add(panel);
		}
		return container;
	},
	
	mkSharedTbar: function(cont) {
		var groupTbar = this.getTopToolbar();
		groupTbar.removeAll();
		
		// only copy toolbar of form and message containers
		if (cont.get(0).getXType() == 'itasks.ttc.form' || cont.get(0).getXType() == 'itasks.ttc.message') {
			// copy top toolbar to shared group toolbar
			var taskTbar = cont.get(0).getTopToolbar();
			if(taskTbar) {
				this.copyTbar(taskTbar, groupTbar);
			}
			
			groupTbar.cascade(function(){
				Ext.apply(this,{
					taskId : cont.get(0).taskId
				});
			});
		}
		
		groupTbar.setVisible(groupTbar.items.length > 0);
	},
	mkGroupAToolbar: function() {
		var groupTbar = this.getTopToolbar();
		groupTbar.removeAll();
		groupTbar.add(this.groupAMenu);
		groupTbar.setVisible(groupTbar.items.length > 0);
	
		itasks.ttc.common.setupHotkeys(groupTbar, this);
		groupTbar.doLayout();
	},
	copyTbar: function(src,dst) {
		var items = src.items.getRange();
		src.removeAll(false);
		dst.removeAll();
		dst.add(items);
		src.doLayout();
		dst.doLayout();
	},
	hideTopGroupActionMenuItems: function(item, options) {
		if(Ext.isBoolean(item.topGroupAction)) {
			// items is a normal menu item
			if (item.topGroupAction) {
				// hide top group actions
				item.hide();
			} else {
				// there is a non-hidden item
				// no separator has to be hidden and the menu can stay enabled
				options.hideNextSeparator = false;
				options.hideSeparatorOnEnd = false;
				options.disableMenu = false;
			}
		} else if (item.getXType() == 'menuseparator') {
			if (options.hideNextSeparator)
				// hide separator
				item.hide();
			else
				// do not hide separator at the moment,
				// but possibly later if it would be the last item of the menu
				options.hideSeparatorOnEnd = item;
				
			// no two consecutive separators
			options.hideNextSeparator = true;
		} else {
			// start of new (sub menu)
			var newOpts = {
				disableMenu: true,
				hideNextSeparator: true,
				hideSeparatorOnEnd: false
			};
				
			var children =  item.items || item.menu.items;

			for(var i = 0; i < children.length; i++) {
				this.hideTopGroupActionMenuItems(children.get(i), newOpts);
			}

			if (item.getXType() != 'toolbar') {
				// possibly hide last separator
				if (newOpts.hideSeparatorOnEnd)
					newOpts.hideSeparatorOnEnd.hide();
				
				// disable empty main and hide empty submenus
				if (newOpts.disableMenu)
					if (item.getXType() == 'button')
						item.disable();
					else
						item.hide();
			}
		}
	},
	
	showAllMenuItems: function(item) {
		if(item.name || item.getXType() == 'menuseparator') {
			// show all normal item & separators
			item.show();
		} else {
			if (item.getXType() == 'button' && item.menu.items.length == 0) {
				// disable empty main menus
				item.disable();
				return;
			} else {
				// show & enable all non-empty main and all submenus
				item.enable();
				item.show();
			}
			
			var children =  item.items || item.menu.items;
			for(var i = 0; i < children.length; i++) {
				this.showAllMenuItems(children.get(i));
			}
		}
	}
});

itasks.ttc.GroupItemWindow = Ext.extend(Ext.Window,{

	initComponent: function() {
		this.width = 750;
		this.height = 300;
	
	
		if(this.items.length > 0 && this.items[0].menu) {
			this.tbar = this.items[0].menu;
		}	
		itasks.ttc.GroupItemWindow.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.ttc.group',itasks.ttc.GroupContainer);