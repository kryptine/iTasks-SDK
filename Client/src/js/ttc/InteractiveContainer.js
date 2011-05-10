Ext.ns('itasks.ttc');

itasks.ttc.InteractiveContainer = Ext.extend(itasks.ttc.TTCBase, {
	autoScroll: true,
	layout: 'hbox',
	dirty: true,
	initComponent : function() {		
		itasks.ttc.InteractiveContainer.superclass.initComponent.apply(this,arguments);
	},
	buildComponents: function(data){
		this.items = data.content;
	},
	update: function(data) {
		if (data == "done" || data == "redundant"){
			this.fadeOut(data);
			return;
		}
		
		if(data.updates) {
			var doLayout = false;
			//errors and hints are updated separately
			var num = data.updates.length;
			for (i = 0; i < num; i++) {
				var update = data.updates[i];
				switch(update[0]) {
					case "TUISetValue":
						if(cmp = this.findComponentByPath(this, update[1])) {
							if(cmp.setValue){
								cmp.setValue(update[2]);
								doLayout = true;
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
					case "TUISetError":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.setError(update[2]);
						}
						break;
					case "TUISetHint":
						if(cmp = this.findComponentByPath(this, update[1])) {
							cmp.setHint(update[2]);
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
							this.dirty = true;
						}
						break;
				}
			}
			
			if (doLayout) this.doLayout(true);
			
		} else {
			//Completely replace form
			itasks.ttc.InteractiveContainer.superclass.update.apply(this,arguments);
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
		var steps = path.split("-");
		var target = parseInt(steps.pop());	
		var cmp = start;
		//Find parent element
		for(var i = 0; i < steps.length; i++) {	
			
			if(cmp.isXType('itasks.tui.Constructor')) {
				cmp = cmp.itemPanel.items.get(parseInt(steps[i]));
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
			itasks.ttc.InteractiveContainer.superclass.doLayout.apply(this,arguments);
			return;
		}
		if (shallow === false) return;
		
		var p = this.findParentByType('itasks.work').get(1);
		var w = p.getWidth();
		var h = p.getHeight() - p.getTopToolbar().getHeight();
		
		this.suspendEvents();
		this.setSize(w,h);
		this.resumeEvents();
		
		if (this.dirty) {
			itasks.ttc.InteractiveContainer.superclass.doLayout.call(this);
			this.dirty = false;
		}
		
		itasks.tui.cache = {};
		this.get(0).doTUILayout(w, h);

		itasks.ttc.InteractiveContainer.superclass.doLayout.call(this);
	}
});

Ext.reg('itasks.ttc.interaction',itasks.ttc.InteractiveContainer);