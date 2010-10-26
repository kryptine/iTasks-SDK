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
			
			if(this.content[i].behaviour == 'Floating' || this.content[i].behaviour == 'Modal') {
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
				
		//Loop over content and update/add child items	
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
				cont = this.createContainer(panel, behaviour, index);
				
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
		
		this.menu = data.menu;
		var bbar = this.getBottomToolbar();
		bbar.removeAll();
		bbar.add(data.bbar);
		this.doLayout();
	},
	updateItem: function(container, panel, behaviour, index) {
		var oldPanel = container.get(0);
		if(oldPanel.getXType() == panel.xtype && oldPanel.taskId == panel.taskId) {
			// update container contents
			oldPanel.update(panel);
			
			var tbar = container.getTopToolbar();
			if (tbar) {
				tbar.removeAll();
				tbar.add(oldPanel.menu);
			}
		} else {
			//if not same xtype or taskId - completely replace container contents
			container.removeAll();
			container.add(panel);
		}
		return container;
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