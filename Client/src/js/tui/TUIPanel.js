Ext.ns('itasks.tui');

itasks.tui.TUIPanel = Ext.extend(Ext.Container, {
	autoScroll: true,
	layout: 'hbox',
	dirty: true,
	initComponent : function() {
		this.items = this.content;
		Ext.apply(this, {
			taskUpdates : {},
			url: itasks.config.serverUrl + '/work/tab',
			unstyled: true
		});
		
		itasks.tui.TUIPanel.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('taskRedundant','taskDone');
		this.enableBubble('taskRedundant','taskDone');
	},
	buildComponents: function(data){
		
	},
	update: function(data) {
		if (data == "done" || data == "redundant"){
			this.fadeOut(data);
			return;
		}
		
		if(data.updates) {
			this.suspendEvents();
			var doLayout = false;
			var num = data.updates.length;
			for (i = 0; i < num; i++) {
				var update = data.updates[i];
				switch(update[0]) {
					case "TUISetValue":
						if(cmp = this.findComponentByPath(this, update[1])) {
							if(cmp.setValue){
								cmp.setValue(update[2]);
							}else{
								cmp.value = update[2]; // cmp is not created yet
							}
						}
						break;
					case "TUISetTaskId":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.taskId = update[2];
						}
						break;
					case "TUISetName":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.name = update[2];
						}
						break;
					case "TUISetEnabled":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.setDisabled(!update[2]);
						}
						break;
					case "TUISetTitle":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.setTitle(update[2][0]);
							cmp.setIconClass(update[2][1]);
						}
						break;
					case "TUISetSize":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.setTuiWidth(update[2]);
							cmp.setTuiHeight(update[3]);
							doLayout = true;
						}
						break;
					case "TUIReplace":
						cmp = this.replaceComponentByPath(this, update[1], update[2]);
						doLayout = true;
						this.dirty = true;
						break;
					case "TUIUpdate":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.update(update[2]);
						}
						break;
					case "TUIAdd":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.insert(update[2],update[3]);
							doLayout = true;
							this.dirty = true;
						}
						break;
					case "TUIRemove":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.remove(update[2]);
							doLayout = true;
						}
						break;
				}
			}
			this.resumeEvents();
			
			if (doLayout) this.doLayout(true);
			
		} else {
			//Completely replace form
			this.menu = data.menu;
			this.removeAll();
			this.add(data.content);
			
			this.dirty = true;
			this.doLayout();
		}

		this.menu = data.menu;
	},
	findComponentByPath: function(start, path) {
		var path = path.split("-");
		var cmp = start;
		for(var i = 0; i < path.length; i++) {
			var steps = parseInt(path[i]);
			
			if (cmp.xtype == 'itasks.tui.Grid') {
				//Get grid editor
				var row = parseInt(path[i+1]);
				if (cmp.gridEditors[row].length != 1){
					var col = parseInt(path[i+2]);
					i = i + 2;
					cmp = cmp.gridEditors[row][col];
				}else{
					i++;
					cmp = cmp.gridEditors[row][0];
				}
			} else if(cmp.xtype == 'itasks.tui.Constructor') {
				cmp = cmp.itemPanel.items.get(steps);
			} else if(cmp.isXType('itasks.tui.MainContainer')) {
				//Choose between menu or items
				if(steps == 0) {
					cmp = cmp.get(1).getTopToolbar();
				} else {
					cmp = cmp.get(1).items.get(parseInt(path[i+1]));
					i++;
				}
			} else {
				cmp = cmp.items.get(steps);
			}
			if(cmp.xtype == 'itasks.tui.list.Item') {
				//Skip list items in the counting
				cmp = cmp.items.get(0);
			}
			if(!cmp) {
				return null;
			}
		}

		return cmp;
	},
	replaceComponentByPath: function(start, path, replacement) {
		
		var cmp = start;
		//Find parent element
		var steps = path.split("-");
		for(var i = 0; i < steps.length - 1; i++) {	
			
			if(cmp.isXType('itasks.tui.Constructor')) {
				cmp = cmp.itemPanel.items.get(parseInt(steps[i]));
			} else if(cmp.isXType('itasks.tui.MainContainer')) {
				if(parseInt(steps[i]) == 0) {
					cmp = cmp.get(1).getTopToolbar();
				} else {
					cmp = cmp.get(1).items.get(parseInt(steps[i+1]));
					i++;
				}
			} else {
				cmp = cmp.items.get(parseInt(steps[i]));
			}
			
			if(!cmp) {
				return null;
			}
			//Skip list items in the counting
			if(cmp.isXType('itasks.tui.list.Item')) {
				cmp = cmp.items.get(0);
				if(!cmp) {
					return null;
				}
			}	
		}
		//Determine target
		var target = parseInt(steps.pop());	
		
		//Update component
		if(cmp.isXType('itasks.tui.Constructor')) {
			cmp.itemPanel.remove(target);
			cmp.itemPanel.insert(target, replacement);
		} else if (cmp.isXType('itasks.tui.List')) {
			var itemCtrl = cmp.items.get(target);
			itemCtrl.remove(0);
			itemCtrl.add(replacement);
		} else {
			cmp.remove(target);
			cmp.insert(target, replacement);	
		}
		
		return cmp.items.get(target);
	},
	
	doLayout: function(shallow) {
		if (!Ext.isFunction(this.get(0).doTUILayout)) {
			itasks.tui.TUIPanel.superclass.doLayout.apply(this,arguments);
			return;
		}
		if (shallow === false) return;

		var p = this.findParentByType('itasks.work');
		var w = p.getWidth();
		var h = p.getHeight();

		this.suspendEvents();
		this.cascade(function() {this.show();}); // show all child components to prevent errors
		this.setSize(w,h);
		this.resumeEvents();
		
		if (this.dirty) {
			itasks.tui.TUIPanel.superclass.doLayout.call(this);
			this.dirty = false;
		}

		itasks.tui.cache = {};
		if (!Ext.isDefined(itasks.tui.permCache)) itasks.tui.permCache = {};
		this.get(0).doTUILayout(w,h);

		itasks.tui.TUIPanel.superclass.doLayout.call(this);
	},
	
	fadeOut: function(data) {
		if(data == "redundant"){
			msg = "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort.";
			this.fireEvent("taskRedundant");
		}else{
			msg = "This task is completed. Thank you.";
			this.fireEvent("taskDone");
		}
		
		var height = (this.interactionpanel ? this.interactionpanel.getHeight() : 0);
		this.removeAll();
		this.add({
			xtype: "itasks.finished",
			title: "Task completed",
			html: msg,
			destroyCmp: this.findParentByType("itasks.work")
		});
	
		this.doLayout();
	}
});

Ext.reg('itasks.tui.panel',itasks.tui.TUIPanel);