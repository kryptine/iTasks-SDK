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
		
		// add undock button to toolbar for (un)dockable tasks
		if((behaviour == 'GBFixed' || behaviour == 'GBFloating') && cont.content) {
			var undockButton = {
				iconCls: 'icon-unpin',
				cls: 'GroupToolbarUndockControls',
				handler: function() {
					var panel = group.focusedContainer;
					var pos = group.items.indexOf(panel);
					delete group.focusedContainer;
					var cont = panel.get(0);
					panel.removeAll(false);
					panel.destroy();
					// copy toolbar back from shared one
					group.copyTbar(group.getTopToolbar(), cont.getTopToolbar());
					group.addContainer(cont, 'GBFloating', panel.id, false, pos);
					group.doLayout();
					group.focusFirstContainer();
				}
			};
			var tbar = cont.content.tbar;
			tbar[tbar.length] = {xtype: 'tbseparator', cls: 'GroupToolbarUndockControls'};
			tbar[tbar.length] = undockButton;
		}

		switch(behaviour) {
			case 'GBFixed':
			case 'GBAlwaysFixed':
				var panel = {
					xtype: 'panel',
					cls: 'GroupFixed GroupFixedNoFocus',
					id: id,
					items: [cont],
					unstyled: true,
					listeners: {
						afterrender: function(p) {
							p.el.on('mousedown', function() {
								group.focusContainer(p);
							});
							
							if(focus)
								group.focusContainer(p);
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
					}
				};
				break;
			case 'GBFloating':
				var tools = [{
					id: 'pin',
					handler: function(ev,el,window) {
						var pos = group.items.indexOf(window);
						var cont = window.get(0);
						window.removeAll(false);
						window.destroy();
						group.addContainer(cont, 'GBFixed', window.id, true, pos);
						group.doLayout();
						cont.getTopToolbar().doLayout();
					}
				}];
			case 'GBAlwaysFloating':
				var panel = {
					xtype: 'window',
					cls: 'GroupFloating',
					id: id,
					initHidden: false,
					closable: false,
					shadow: false,
					items: [cont],
					title: cont.description || "No Description",
					tools: tools
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
	
		var groupTbar = this.getTopToolbar();

		if(Ext.isDefined(this.focusedContainer)) {
			// copy top toolbar back to container
			var prevTbar = this.focusedContainer.get(0).getTopToolbar();
			this.copyTbar(groupTbar, prevTbar);
			this.focusedContainer.unfocusFixed();
		} else {
			groupTbar.removeAll();
		}

		// copy top toolbar to shared group toolbar
		var taskTbar = cont.get(0).getTopToolbar();
		if(taskTbar)
			this.copyTbar(taskTbar, groupTbar);
		groupTbar.setVisible(groupTbar.items.length > 0);

		//apply the taskId to the new TB, so that attachTaskHandlers nows on which subtask the actions should
		//be applied
		groupTbar.cascade(function(){
			Ext.apply(this,{
				taskId : cont.get(0).taskId
			});
		});
		itasks.ttc.common.attachTaskHandlers(groupTbar,cont.get(0).taskId);

		// focus container
		cont.focusFixed();
		this.focusedContainer = cont;
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