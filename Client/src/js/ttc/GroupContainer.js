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
		
		this.content = this.filterContent(this.content);
		
		for(var i=0; i < this.content.length; i++) {
			this.addContainer(this.content[i].panel,this.content[i].behaviour,this.content[i].index);
		}
	},
	
	afterRender: function() {
		itasks.ttc.GroupContainer.superclass.afterRender.call(this,arguments);
		this.focusFirstContainer();
	},
	
	addContainer: function(cont,behaviour,idx,pos) {
		var group = this;
		var id = this.mkElementId(idx);
		switch(behaviour) {
			case 'AlwaysFixed':
				this.storeButtonsAndMenus(cont);
				var panel = {
					xtype: 'panel',
					id: id,
					items: [cont],
					unstyled: true,
					cls: 'ttc-no-focus',
					focused: false,
					listeners: {
						afterrender: function(p) {
							p.el.on('mousedown', function() {
								group.focusContainer(p);
							});
						}
					},
					focusFixed: function() {this.focused = true; this.removeClass('ttc-no-focus');},
					unfocusFixed: function() {this.focused = false; this.removeClass('ttc-no-focus');this.addClass('ttc-no-focus');}
				};
				break;
			case 'AlwaysFloating':
				this.configWindowContent(cont);
				var panel = {
					xtype: 'window',
					id: id,
					initHidden: false,
					closable: false,
					shadow: false,
					shadow: false,
					items: [cont],
					title: cont.description || "No Description"
				};
				break;
			}
			
		if (pos)
			this.insert(pos,panel);
		else
			this.add(panel);
	},
	
	focusContainer: function(cont) {
		if(Ext.isDefined(this.focusedContainer))
			this.focusedContainer.unfocusFixed();
	
		cont.focusFixed();
		this.focusedContainer = cont;
		
		this.refreshToolbar(this.getTopToolbar(), this.containerMenus, cont);
		this.refreshToolbar(this.getBottomToolbar(), this.containerButtons, cont);
	},
	
	focusFirstContainer: function() {
		var fixedExisting = false;
		for(var i=0; i < this.items.length; i++){
			if(this.get(i).getXType() != "window") {
				this.focusContainer(this.get(i));
				fixedExisting = true;
				break;
			}
		}
		
		if(!fixedExisting) {
			this.getTopToolbar().hide();
			this.getBottomToolbar().hide();
		}
	},
	
	refreshToolbar: function(tb, contentCollection, focusedCont) {
		tb.removeAll();
		var newTb = contentCollection.get(focusedCont.get(0).taskId);
		if(newTb)
			tb.add(newTb);
		tb.setVisible(tb.items.length > 0);
		tb.doLayout();
		//itasks.ttc.FormContainer.prototype.attachTaskHandlers(tb);
		
		//apply the taskId to the new TB, so that attachTaskHandlers nows on which subtask the actions should
		//be applied
		tb.cascade(function(){
			Ext.apply(this,{
				taskId : focusedCont.get(0).taskId
			});
		});
		
		itasks.ttc.common.attachTaskHandlers(tb,focusedCont.get(0).taskId);
	},
	
	removeContainer: function(i) {
		var cont = this.get(i);
		if(cont.focused && cont.getXType() != 'Window') {
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
	
	filterContent: function(content) {
		return content.filter(function (val) {
			if(val.panel == "done" || val.panel == "redundant") return false;
			else return true;
		});
	},
	
	update: function(data) {
		var content = this.filterContent(data.content);

		for(var i=0; i < content.length; i++){
			var data = content[i].panel;
		
			for(var j=i; j < this.items.length; j++) {
				if(this.mkElementId(content[i].index) == this.get(j).id) break;
			}

			for(var k=0; k < (j-i); k++) {
				this.removeContainer(i);
			}
			
			if(i < this.items.length){
				var cont = this.get(i).get(0);
				var isWindow = this.get(i).getXType() == "window";
				if(isWindow) this.configWindowContent(data);
				
				if(cont.getXType() == data.xtype){
					if (!isWindow) this.storeButtonsAndMenus(data);
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
					if (!isWindow) this.storeButtonsAndMenus(data);
					this.get(i).add(data);
				}
			} else {
				this.addContainer(data,content[i].behaviour,content[i].index,j);
			}
		}
		
		var trailing = this.items.length - content.length;
		for(var i=0; i<trailing; i++){
			this.removeContainer(this.items.length-1);
		}
		
		// focus first fixed container if focused one is deleted
		if(!Ext.isDefined(this.focusedContainer))
			this.focusFirstContainer();
		
		this.doLayout();
	},
	
	addUpdate: function(name, value) {
		this.focusedContainer.get(0).addUpdate(name, value);
	},
	
	sendUpdates: function(delay) {
		this.focusedContainer.get(0).sendUpdates(delay);
	},
	
	configWindowContent: function(cont) {
		if(cont.content) {
			var noTbar = true;
			for(var i=0; i < cont.content.tbar.length; i++){
				if(!cont.content.tbar[i].disabled) {
					noTbar = false;
					break;
				}
			}
			if (noTbar)
				cont.content.tbar = [];
		}
			
		cont.hideDescription = true;
	},
	
	mkElementId: function(idx) {
		return 'groupEl-' + this.taskId + '-' + idx;
	}
});

Ext.reg('itasks.ttc.group',itasks.ttc.GroupContainer);