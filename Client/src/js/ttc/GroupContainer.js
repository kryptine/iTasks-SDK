Ext.ns('itasks.ttc')

itasks.ttc.GroupContainer = Ext.extend(Ext.Panel,{

	initComponent: function(){
		Ext.apply(this, 
		{ layout:'auto'
		, autoScroll: true
		, cls: 'GroupContainer'
		, unstyled: true
		, tbar: []
		});

		itasks.ttc.GroupContainer.superclass.initComponent.apply(this,arguments);
		
		this.content = this.filterContent(this.content);
		
		for(var i=0; i < this.content.length; i++) {
			this.addContainer(this.content[i].panel,this.content[i].behaviour,this.content[i].index,false);
		}
	},
	
	onLayout: function() {
		itasks.ttc.GroupContainer.superclass.onLayout.call(this,arguments);
		if(!Ext.isDefined(this.focusedContainer))
			this.focusFirstContainer();
	},
	
	addContainer: function(cont,behaviour,id,focus,pos) {
		var group = this;
		if(Ext.isNumber(id))
			id = this.mkElementId(id);

		switch(behaviour) {
			case 'GBFixed':
				var undockable = true;
			case 'GBAlwaysFixed':
				var panel = {
					xtype: 'panel',
					cls: 'GroupFixed GroupFixedNoFocus',
					id: id,
					items: [cont],
					unstyled: true,
					disabled: this.modalDlg,
					listeners: {
						afterrender: function(p) {
							p.el.fadeIn();
							p.el.on('mousedown', function() {
								group.focusContainer(p);
							});
							
							if(focus) {
								p.el.frame();
								group.focusContainer(p);
							}
						}
					},
					focusFixed: function() {
						this.removeClass('GroupFixedNoFocus');
						this.doLayout();
					},
					unfocusFixed: function() {
						this.removeClass('GroupFixedNoFocus');
						this.addClass('GroupFixedNoFocus');
						this.doLayout();
					},
					undockable: undockable
				};
				break;
			default:
				if (behaviour == 'GBFloating')
					var tools = [{
						id: 'pin',
						handler: function(ev,el,window) {
							window.getEl().fadeOut({callback: function() {
								var pos = group.items.indexOf(window);
								var cont = window.get(0);
								window.removeAll(false);
								window.destroy();
								group.addContainer(cont, 'GBFixed', window.id, true, pos);
								group.doLayout();
							}});
						}
					}];
				else if(behaviour == 'GBModal') {
					this.disable();
					// also disable all windows
					this.items.each(function(cont) {
						cont.disable();
					});
					this.modalDlg = true;
				}
					
				var panel = {
					xtype: 'window',
					cls: 'GroupFloating',
					id: id,
					initHidden: false,
					closable: false,
					resizable: false,
					shadow: false,
					items: [cont],
					title: cont.description || "No Description",
					tools: tools,
					disabled: this.modalDlg && behaviour != 'GBModal',
					listeners: {
						afterrender: function(p) {
							if(behaviour == 'GBModal')
								p.toFront();
							else
								p.toBack();
							p.el.fadeIn();
						},
						destroy: function(){
							if(behaviour == 'GBModal') {
								this.enable();
								this.items.each(function(cont) {
									cont.enable();
								});
								this.modalDlg = false;
							}
						}.createDelegate(this)
					}
				};
				break;
			}
			
		if (Ext.isDefined(pos))
			this.insert(pos,panel);
		else
			this.add(panel);
	},
	
	focusContainer: function(cont) {
		if(cont == this.focusedContainer)
			return;
			
		if(Ext.isDefined(this.focusedContainer)) {
			// copy top toolbar back to container
			var prevTbar = this.focusedContainer.get(0).getTopToolbar();
			if (prevTbar) {
				var groupTbar = this.getTopToolbar();
				groupTbar.items.each(function(item) {
					if(item.undockControl)
						item.destroy();
				});
				this.copyTbar(groupTbar, prevTbar);
			}
			this.focusedContainer.unfocusFixed();
		}
	
		this.mkSharedTbar(cont);

		// focus container
		cont.focusFixed();
		this.focusedContainer = cont;
	},
	
	mkSharedTbar: function(cont) {
		var groupTbar = this.getTopToolbar();
		
		groupTbar.removeAll();

		// copy top toolbar to shared group toolbar
		var taskTbar = cont.get(0).getTopToolbar();
		if(taskTbar) {
			this.copyTbar(taskTbar, groupTbar);
		}
		
		if(cont.undockable) {
			// add undock button to toolbar for undockable tasks
			var group = this;
			var undockButton = {
				iconCls: 'icon-unpin',
				cls: 'GroupToolbarUndockControls',
				handler: function() {
					var panel = group.focusedContainer;
					panel.getEl().fadeOut({callback: function() {
						var pos = group.items.indexOf(panel);
						delete group.focusedContainer;
						var cont = panel.get(0);
						panel.removeAll(false);
						panel.destroy();
						
						// copy toolbar back from shared one
						var contTbar = cont.getTopToolbar();
						if(contTbar) {
							var groupTbar = group.getTopToolbar();
							groupTbar.items.each(function(item) {
								if(item.undockControl)
									item.destroy();
							});
							group.copyTbar(groupTbar, contTbar);
						}
						
						group.addContainer(cont, 'GBFloating', panel.id, false, pos);
						group.doLayout();
						group.focusFirstContainer();
					}});
				},
				undockControl: true
			};
			if(groupTbar.items.length > 0)
				groupTbar.add({xtype: 'tbseparator', undockControl: true});
			groupTbar.add(undockButton);
			groupTbar.doLayout();
		}
		
		groupTbar.setVisible(groupTbar.items.length > 0);

		//apply the taskId to the new TB, so that attachTaskHandlers nows on which subtask the actions should
		//be applied
		groupTbar.cascade(function(){
			Ext.apply(this,{
				taskId : cont.get(0).taskId
			});
		});
		itasks.ttc.common.attachTaskHandlers(groupTbar,cont.get(0).taskId);
	},
	
	copyTbar: function(src,dst) {
		var items = src.items.getRange();
		src.removeAll(false);
		dst.removeAll();
		dst.add(items);
		src.doLayout();
		dst.doLayout();
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
			delete this.focusedContainer;
			this.getTopToolbar().hide();
		}
	},
	
	removeContainer: function(i) {
		var cont = this.get(i);
		if(cont == this.focusedContainer && cont.getXType() != 'Window') {
			delete this.focusedContainer;
		}
		this.remove(i,true);
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
				
				if(cont.getXType() == data.xtype && cont.taskId == data.taskId) {
					cont.update(data);
				} else {
					//if not same xtype or taskId - completely replace container contents
					this.get(i).removeAll();
					this.get(i).add(data);
				}
			} else {
				this.addContainer(data,content[i].behaviour,content[i].index,false,j);
			}
		}
		
		var trailing = this.items.length - content.length;
		for(var i=0; i<trailing; i++){
			this.removeContainer(this.items.length-1);
		}
		
		if(Ext.isDefined(this.focusedContainer) && this.focusedContainer.get(0).getTopToolbar() && this.focusedContainer.get(0).getTopToolbar().items.length > 0) {
			this.mkSharedTbar(this.focusedContainer);
		}
		
		this.doLayout();
	},
	
	addUpdate: function(name, value) {
		this.focusedContainer.get(0).addUpdate(name, value);
	},
	
	sendUpdates: function(delay) {
		this.focusedContainer.get(0).sendUpdates(delay);
	},
	
	mkElementId: function(idx) {
		return 'groupEl-' + this.taskId + '-' + idx;
	}
});

Ext.reg('itasks.ttc.group',itasks.ttc.GroupContainer);