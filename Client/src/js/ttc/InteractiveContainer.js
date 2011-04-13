Ext.ns('itasks.ttc');

itasks.ttc.InteractiveContainer = Ext.extend(itasks.ttc.TTCBase, {
	initComponent : function() {
		this.cls = this.getCls(this.type);
		
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
		
		// update css class
		if(data.type != this.type){
			this.removeClass(this.getCls(this.type));
			this.addClass(this.getCls(data.type));
			this.type = data.type;
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
					case "TUIReplace":
						cmp = this.replaceComponentByPath(this, update[1], update[2]);
						doLayout = true;
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
			
			if (doLayout) this.doLayout();
			
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
	
	getCls: function(type) {
		switch(type){
			case 'Information':	return 'TTCInformationContainer';
			case 'Message':		return 'TTCMessageContainer';
			case 'Instruction':	return 'TTCInstructionContainer';
			case 'Monitor':		return 'TTCMonitorContainer';
			case 'Control':		return 'TTCControlContainer';
			case 'Parallel':	return 'TTCParallelControlContainer';
			case 'Result':		return 'TTCResultContainer';
		}
	}
});

Ext.reg('itasks.ttc.interactive',itasks.ttc.InteractiveContainer);