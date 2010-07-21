Ext.ns('itasks.ttc')

itasks.ttc.GroupContainer = Ext.extend(Ext.Panel,{
	initComponent: function(){
		Ext.apply(this, 
		{ layout:'auto'
		, cls: 'GroupContainer'
		, autoScroll: true
		, unstyled: true
		, tbar: []
		, fixedCont: new Ext.util.MixedCollection()		// collection of all fixed containers
		, floatingCont: new Ext.util.MixedCollection()	// collection of all floating containers
		, taskUpdates : {}
		, url: itasks.config.serverUrl + '/work/tab'
		});

		itasks.ttc.GroupContainer.superclass.initComponent.apply(this,arguments);
		
		for(var i=0; i < this.content.length; i++) {
			this.createContainer(this.content[i].panel,this.content[i].behaviour,this.content[i].index,false);
		}
		
		// add all fixed containers as children, floating containers are rendered in a different way
		this.fixedCont.each(function(cont) {
			this.add(cont);
		}, this);
	},
	
	createContainer: function(cont, behaviour, idx, focus) {
		var group = this;
		
		// check for behaviour stored in cookie for containers which can be (un)pinned
		if (behaviour == 'GBFixed' || behaviour == 'GBFloating')
			behaviour = Ext.state.Manager.get(this.taskId + '_' + idx + '_behaviour', behaviour);
		
		this.setupChildContainer(cont, behaviour);
		
		switch(behaviour) {
			case 'GBFixed':
			case 'GBAlwaysFixed':
				this.fixedCont.add(idx, new Ext.Panel({
					index: idx,
					cls: 'GroupFixed GroupFixedNoFocus',
					items: [cont],
					unstyled: true,
					listeners: {
						afterrender: function(p) {
							//p.el.fadeIn();
							p.el.on('mousedown', function() {
								group.focusContainer(p);
							});
							
							if(focus) {
								//p.el.frame();
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
					}
				}));
				break;
			default:
				if (behaviour == 'GBFloating') {
					// pin button for floating containers
					var tools = [{
						id: 'pin',
						handler: function(ev,el,window) {
							var cont = window.get(0);
							var idx = window.index;
							window.removeAll(false);
							group.floatingCont.remove(window);
							window.destroy();
							Ext.state.Manager.set(group.taskId + '_' + idx + '_behaviour', 'GBFixed');
							group.createContainer(cont, 'GBFixed', idx, true);
							group.showAllMenuItems(cont.getTopToolbar());
							group.renderFixed();
						}
					}];
				}
				
				if(cont.xtype == 'itasks.ttc.proc-control')
					var title = 'Control process properties';
				else if(cont.xtype == 'itasks.ttc.monitor')
					var title = 'Monitor task';
				else
					var title = cont.description || cont.label;
				
				this.floatingCont.add(idx, {
					id: this.taskId + '_' + idx,
					xtype: 'window',
					index: idx,
					cls: 'GroupFloating',
					closable: false,
					autoScroll: true,
					maximizable: true,
					constrainHeader: true,
					modal: behaviour == 'GBModal',
					items: [cont],
					title: title,
					tools: tools,
					minWidth: 300,
					stateful: true,
					stateEvents: ['maximize']
				});
				break;
		}
	},
	
	setupChildContainer: function(cont,behaviour) {
		var group = this;
		if (behaviour == 'GBFixed' || behaviour == 'GBFloating') {
			// add unpin button for undockable containers, is hidden if rendered inside window
			cont.headerButton = {
				xtype:'button', unstyled: true, iconCls: 'icon-unpin', cls: 'GroupUnpinButton',
				handler: function() {
					var panel = group.focusedContainer;
					//panel.getEl().fadeOut({callback: function() {
						group.focusedContainer = null;
						var cont = panel.get(0);
						panel.removeAll(false);
						group.fixedCont.remove(panel);
						panel.destroy();

						// copy toolbar back from shared one & hide menu items referring to top group actions
						if(cont.getXType() == 'itasks.ttc.form' || cont.getXType() == 'itasks.ttc.message') {
							group.copyTbar(group.getTopToolbar(), cont.getTopToolbar());
							group.hideTopGroupActionMenuItems(cont.getTopToolbar());
						}

						// hack: remove fixed width of footer
						/*var footer = Ext.DomQuery.selectNode('.x-plain-footer', cont.getEl().dom);
						if(footer)
							Ext.DomHelper.applyStyles(footer, {width:'auto'});*/
						
						Ext.state.Manager.set(group.taskId + '_' + panel.index + '_behaviour', 'GBFloating');
						group.createContainer(cont, 'GBFloating', panel.index, false);
						group.focusFirstContainer();
						group.doLayout();
					//}});
				}
			};
		}
	},
	
	renderFixed: function() {
		// add new floating containers in order given by index
		var insertPos = 0;
		this.fixedCont.keySort();
		this.fixedCont.eachKey(function(idx,cont) {
			var pos = this.items.indexOf(cont);
			if(pos != -1) {
				insertPos = pos + 1;
			} else {
				this.insert(insertPos, cont);
				insertPos++;
			}
		}, this);
		
		this.doLayout();
	},
	
	renderFloating: function() {
		var modalDlgs = new Ext.util.MixedCollection();
		this.floatingCont.eachKey(function(idx,cont) {
			if (!cont.rendered) {
				cont.renderTo = this.getEl();
				cont = Ext.create(cont);
				
				// hide menu items referring to top group actions
				if(cont.get(0).getXType() == 'itasks.ttc.form' || cont.get(0).getXType() == 'itasks.ttc.message')
					this.hideTopGroupActionMenuItems(cont.get(0).getTopToolbar());
				
				// fix size of window
				var s = cont.getSize();
				cont.setSize(s.width > cont.minWidth ? s.width : cont.minWidth, s.height);
				//hack: make sure the ownerCt is set to reference the parent, so that findParentByType works.
				cont.ownerCt = this;
				
				if(!cont.modal)
					cont.show();
				else
					modalDlgs.add(cont);
				// put created object into collection
				this.floatingCont.replace(idx,cont);
			}
		}, this);
		
		// show modal dialogs after normal ones to make sure they are rendered in front
		modalDlgs.each(function(dlg) {
			dlg.show();
		});
	},
	
	onLayout: function() {
		itasks.ttc.GroupContainer.superclass.onLayout.call(this,arguments);
		// render windows after layouting to make sure they are positioned inside container correctly
		this.renderFloating();
		// focus first container the first time group is layouted
		if(!Ext.isDefined(this.focusedContainer))
			this.focusFirstContainer();
	},
	
	focusContainer: function(cont) {
		if(cont == this.focusedContainer)
			return; // cont already focused
			
		var oldFocused = this.focusedContainer;
		this.focusedContainer = cont;
			
		if(oldFocused) {
			oldFocused.unfocusFixed();
			
			// only copy toolbar of form and message containers
			if(oldFocused.get(0).getXType() == 'itasks.ttc.form' || oldFocused.get(0).getXType() == 'itasks.ttc.message') {
				// copy top toolbar back to container
				this.copyTbar(this.getTopToolbar(), oldFocused.get(0).getTopToolbar());
			}
		}

		// make new toolbar & focus container
		this.mkSharedTbar(cont);
		cont.focusFixed();
	},
	
	focusFirstContainer: function() {
		if (this.fixedCont.getCount() > 0) {
			this.focusContainer(this.fixedCont.itemAt(0));
		} else {
			// there is no fixed cont, create toolbar with group-actions
			this.focusedContainer = null;
			this.mkGroupAToolbar();
		}
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
			
			//apply the taskId to the new TB, so that attachTaskHandlers nows on which subtask the actions should
			//be applied
			groupTbar.cascade(function(){
				Ext.apply(this,{
					taskId : cont.get(0).taskId
				});
			});
			itasks.ttc.common.attachTaskHandlers(groupTbar,cont.get(0).taskId);
		}
		
		groupTbar.setVisible(groupTbar.items.length > 0);
	},
	
	mkGroupAToolbar: function() {
		var groupTbar = this.getTopToolbar();
		groupTbar.removeAll();
		groupTbar.add(this.groupAMenu);
		groupTbar.setVisible(groupTbar.items.length > 0);
		//apply the taskId to the new TB, so that attachTaskHandlers nows on which subtask the actions should
		//be applied
		groupTbar.cascade(function(){
			Ext.apply(this,{
				taskId : this.taskId
			});
		});
		itasks.ttc.common.attachTaskHandlers(groupTbar,this.taskId);
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
	
	update: function(data) {
		var content = data.content;
		this.groupAMenu = data.groupAMenu;
		
		// make copy of old collection of containers to keep track of non-updated ones
		var oldFixed = this.fixedCont;
		this.fixedCont = new Ext.util.MixedCollection();
		var oldFloating = this.floatingCont;
		this.floatingCont = new Ext.util.MixedCollection();
		var focusFixed;
		var focusFloating = new Ext.util.MixedCollection();

		for(var i=0; i < content.length; i++) {
			if(cont = oldFixed.key(content[i].index)) {
				// update existing fixed container
				this.updateItem(cont, content[i].panel, content[i].behaviour);
				oldFixed.remove(cont);
				if (content[i].focus)
					focusFixed = cont;
			} else if(cont = oldFloating.key(content[i].index)) {
				// update existing floating container
				this.updateItem(cont, content[i].panel, content[i].behaviour);
				oldFloating.remove(cont);
				if (content[i].focus)
					focusFloating.add(cont);
			} else {
				// create new container
				this.createContainer(content[i].panel, content[i].behaviour, content[i].index);
			}
		}
		
		// destroy removed containers
		oldFixed.each(function(item) {
			if(item == this.focusedContainer)
				this.focusedContainer = null;
			item.destroy();
		}, this);
		oldFloating.each(function(item) {
			item.destroy();
		});
		
		this.renderFixed();
		if (this.focusedContainer && this.focusedContainer.get(0).getTopToolbar().items.length > 0) {
			// toolbar of focused container has been updated, copy changes to shared toolbar
			this.mkSharedTbar(this.focusedContainer);
		} 
		
		// focus containers for which focus commands have been sent
		if (Ext.isDefined(focusFixed))
			this.focusContainer(focusFixed);
			
		focusFloating.each(function(c) {
			c.toFront();
		}, this);
		
		// try to focus new fixed container
		if(!this.focusedContainer)
			this.focusFirstContainer();
	},
	
	updateItem: function(old, newC, behaviour) {
		var oldC = old.get(0);
		this.setupChildContainer(newC, behaviour);
		if(oldC.getXType() == newC.xtype && oldC.taskId == newC.taskId) {
			// update container contents
			oldC.update(newC);
		} else {
			//if not same xtype or taskId - completely replace container contents
			old.removeAll();
			old.add(newC);
		}
		old.doLayout();
		// put changed container into collections
		if(old.getXType() == 'window')
			this.floatingCont.add(old.index,old);
		else
			this.fixedCont.add(old.index,old);
	},
	
	addUpdate: function(name, value) {
		if(this.focusedContainer)
			// update is handled by focused child
			this.focusedContainer.get(0).addUpdate(name, value);
		else
			// updates for groupAToolbar are handled by group container
			this.taskUpdates[name] = value;
	},
	
	sendUpdates: function(delay) {
		if(this.focusedContainer)
			// update is handled by focused child
			this.focusedContainer.get(0).sendUpdates(delay);
		else {
			// updates for groupAToolbar are handled by group container
			var wt = this.findParentByType(itasks.WorkPanel);
			if(!wt) return;
			wt.sendTaskUpdates(this.taskId,this.taskUpdates);
			this.taskUpdates = {};
		}
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

Ext.reg('itasks.ttc.group',itasks.ttc.GroupContainer);