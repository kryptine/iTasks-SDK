Ext.ns('itasks.ttc')

itasks.ttc.GroupContainer = Ext.extend(Ext.Panel,{

	initComponent: function(){
		this.containerMenus = new Ext.util.MixedCollection();
		this.containerButtons = new Ext.util.MixedCollection();
		
		Ext.apply(this, 
		{ layout:'auto'
		, autoScroll: true
		, cls: 'GroupContainer'
		, unstyled: true
		, tbar: []
		, bbar: []
		, taskUpdates: {}
		});

		itasks.ttc.GroupContainer.superclass.initComponent.apply(this,arguments);
		
		for(var i=0; i < this.content.length; i++) {
			this.addContainer(this.content[i]);
		}
	},
	
	afterRender: function() {
		itasks.ttc.GroupContainer.superclass.afterRender.call(this,arguments);
		this.focusContainer(0);
	},
	
	addContainer: function(cont,pos) {
		var group = this;
		this.storeButtonsAndMenus(cont);
		var panel = new Ext.Panel({
			xtype: 'panel',
			items: [cont],
			unstyled: true,
			cls: 'ttc-no-focus',
			focused: false,
			listeners: {
				afterrender: function(p) {
					p.el.on('mousedown', function() {
						group.focusContainer(group.items.findIndex('id', this.id));
					});
				}
			},
			focus: function() {this.focused = true; this.removeClass('ttc-no-focus');},
			unfocus: function() {this.focused = false; this.removeClass('ttc-no-focus');this.addClass('ttc-no-focus');}
		});
		
		if (pos)
			this.insert(pos,panel);
		else
			this.add(panel);
	},
	
	focusContainer: function(i) {
		if (this.items.length == 0)
			return;
	
		if(this.items.length <= i){
			this.focusContainer(this.items.length-1);
		} else {
			var focusedCont = this.get(i);
			
			if(focusedCont.focused)
				return; // container already focused

			// unfocus previous container
			if (Ext.isDefined(this.focusedContainer)) {
				var cont = this.get(this.focusedContainer);
				if(cont) {
					cont.unfocus();
					delete this.focusedContainer;
				}
			}
			
			// focus new container
			focusedCont.focus();
			this.focusedContainer = i;
			
			this.refreshToolbar(this.getTopToolbar(), this.containerMenus, focusedCont);
			this.refreshToolbar(this.getBottomToolbar(), this.containerButtons, focusedCont);
		}
	},
	
	refreshToolbar: function(tb, contentCollection, focusedCont) {
		tb.removeAll();
		var newTb = contentCollection.get(focusedCont.get(0).taskId);
		if(newTb)
			tb.add(newTb);
		tb.setVisible(tb.items.length > 0);
		tb.doLayout();
		itasks.ttc.FormContainer.prototype.attachTaskHandlers(tb);
	},
	
	removeContainer: function(i) {
		var cont = this.get(i);
		if(cont.focused) {
			this.prevFocusedContainer = this.focusedContainer;
			delete this.focusedContainer;
		}
		this.containerButtons.remove(cont.taskId);
		this.containerMenus.remove(cont.taskId);
		this.remove(i,true);
	},
	
	storeButtonsAndMenus: function(cont) {
		if(cont.content) {
			// store buttons/menus and remove them from container
			this.containerButtons.add(cont.taskId, cont.content.buttons);
			delete cont.content.buttons;
			this.containerMenus.add(cont.taskId, cont.content.tbar);
			cont.content.tbar = [];
		}
	},
	
	update: function(data) {
		var content = data.content;
	
		content = content.filter(function (val) { 
			if(val == "done" || val == "redundant") return false;
			else return true;
		});

		for(var i=0; i < content.length; i++){
			for(var j=i; j < this.items.length; j++){
				if(content[i].taskId == this.get(j).get(0).taskId) break;
			}

			for(var k=0; k < (j-i); k++){
				this.removeContainer(i);
			}
			
			var data = content[i];
			
			if(i < this.items.length){
				var cont = this.get(i).get(0);
				
				if (cont.getXType() == data.xtype){
					this.storeButtonsAndMenus(data);
					cont.update(data);
					if (data.updates) {
						// update disabled-flag in stored buttons/menus
						function updateToolbar(tb) {
							if(!tb)
								return;
						
							if(Ext.isArray(tb)) {
								for(var i=0; i < tb.length; i++){
									updateToolbar(tb[i]);
								}
							} else {
								if(Ext.isDefined(tb.id) && Ext.isDefined(tb.disabled)) {
									function findSetEnabledUpdate(id, def) {
										var num = data.updates.length;
										for (i = 0; i < num; i++) {
											var update = data.updates[i];
											
											if(update[0] == "TUISetEnabled" && update[1] == id)
												return !update[2];
										}
										
										return def;
									}
									tb.disabled = findSetEnabledUpdate(tb.id, tb.disabled);
								}
								if(tb.menu)
									updateToolbar(tb.menu);
								if(tb.items)
									updateToolbar(tb.items);
							}
						}
						updateToolbar(this.containerButtons.get(cont.taskId));
						updateToolbar(this.containerMenus.get(cont.taskId));
					}
				}else{
					//if not same xtype - completely replace container contents
					this.get(i).removeAll();
					this.storeButtonsAndMenus(data);
					this.get(i).add(data);
				}
			}else{
				this.addContainer(data,j);
			}
		}
		
		var trailing = this.items.length - content.length;
		for(var i=0; i<trailing; i++){
			this.removeContainer(this.items.length-1);
		}
		
		// focus new container if focused one is deleted
		if(!Ext.isDefined(this.focusedContainer))
			this.focusContainer(this.prevFocusedContainer);
		this.doLayout();
	},
	
	addUpdate: function(name, value) {
		this.get(this.focusedContainer).get(0).addUpdate(name, value);
	},
	
	sendUpdates: function(delay) {
		this.get(this.focusedContainer).get(0).sendUpdates(delay);
	}
});

Ext.reg('itasks.ttc.group',itasks.ttc.GroupContainer);