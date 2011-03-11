Ext.ns('itasks.ttc');

itasks.ttc.InteractiveContainer = Ext.extend(itasks.ttc.TTCBase, {
	//hidden: true,
	initComponent : function() {
		switch(this.type){
			case 'Information':
				this.cls		= 'TTCInformationContainer';
				this.panelCls	= 'TTCInformationPanel';
				break;
			case 'Message':
				this.cls		= 'TTCMessageContainer';
				this.panelCls	= 'TTCMessagePanel';
				break;
			case 'Instruction':
				this.cls		= 'TTCInstructionContainer';
				this.panelCls	= 'TTCInstructionContainer-Context';
				break;
			case 'Monitor':
				this.cls		= 'TTCMonitorContainer';
				this.panelCls	= 'MonitorPanel';
				break;
		}
		
		itasks.ttc.InteractiveContainer.superclass.initComponent.apply(this,arguments);
		
		this.addEvents("taskRedundant","taskDone");
		this.enableBubble("taskRedundant","taskDone");
	},
	buildComponents: function(data){
		this.interactionpanel = {
			xtype: 'panel',
			cls: this.panelCls,
			layout: 'form',
			unstyled: true,
			autoScroll: true,
			items: data.content.form,
			buttons: data.content.buttons
		};
	},
	update: function(data) {
		//var content = data.content;
		if (data == "done" || data == "redundant"){
			if(data == "redundant"){
				msg = "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort.";
				this.fireEvent("taskRedundant");
			}else{
				msg = "This task is completed. Thank you.";
				this.fireEvent("taskDone");
			}
			
			var par = this.findParentByType("itasks.ttc.parallel");
			if(par){
				var destroyCmp = this;
			}else{
				var destroyCmp = this.findParentByType("itasks.work");
			}
			
			var height = this.descriptionpanel.getHeight() + this.interactionpanel.getHeight();
			this.removeAll();
			this.add({
				xtype: "itasks.ttc.finished",
				subject: "Task completed",
				description: msg,
				descriptionHeight: height,
				destroyCmp: destroyCmp
			});
			
			this.getEl().fadeOut(
				{ duration: itasks.ttc.TTC_FADE_DURATION
				, useDisplay: true
				}
			);
		
			this.doLayout();
		} else if(data.updates) {
			//errors and hints are updated separately
			var num = data.updates.length;
			for (i = 0; i < num; i++) {
				var update = data.updates[i];
				switch(update[0]) {
					case "TUIAdd":
						this.addComponent(update[1],update[2]);
						break;
					case "TUIAddTo":
						this.addComponentTo(update[1],update[2]);
						break;					
					case "TUIRemove":
						this.removeComponent(update[1]);
						break;
					case "TUIReplace":
						this.replaceComponent(update[1],update[2]);						
						break;
					case "TUISetEnabled":
						this.enableComponent(update[1],update[2]);
						break;
					case "TUIReplaceMenu":
						this.menu = update[1];
						break;
					case "TUIReplaceButtons":
						var fbar = this.interactionpanel.getFooterToolbar();
						fbar.removeAll();
						fbar.add(update[1]);
						fbar.doLayout();
						break;						
					//New instructions:
					case "TUISetValue_":
						if(cmp = this.findComponentByPath(this.interactionpanel, update[1])) {
							if(cmp.setValue){
								cmp.setValue(update[2]);
							}else{
								cmp.value = update[2]; // cmp is not created yet
							}
						}
						break;
					case "TUISetError_":
						if(cmp = this.findComponentByPath(this.interactionpanel, update[1])) {
							cmp.setError(update[2]);
						}
						break;
					case "TUISetHint_":
						if(cmp = this.findComponentByPath(this.interactionpanel, update[1])) {
							cmp.setHint(update[2]);
						}
						break;
					case "TUIReplace_":
						cmp = this.replaceComponentByPath(this.interactionpanel, update[1], update[2]);
						break;
					case "TUIUpdate_":
						if(cmp = this.findComponentByPath(this.interactionpanel, update[1])) {
							cmp.update(update[2]);
						}
						break;
				}
			}
		} else {
			//Completely replace form
			itasks.ttc.InteractiveContainer.superclass.update.apply(this,arguments);
		}
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
		
		cmp.doLayout();
		
		return cmp.items.get(target);
	},
	addComponent :  function (id, cmp){
		var ct = Ext.getCmp(id);
		var find = function(cmt,cnt,ind) {
			if(cnt.items.get(ind) == undefined)
				return ind;
			if(cnt.items.get(ind) == cmt)
				return ind;
			else
				return find(cmt,cnt,ind + 1);
		}
		
		if(!ct) return;

		var index = find(ct, ct.ownerCt, 0) + 1;
		var newct = ct.ownerCt.insert(index, cmp);

		ct.ownerCt.ownerCt.doLayout();
	},
	
	addComponentTo : function(parent, cmp){
		var ct = Ext.getCmp(parent);
		if(!ct) return;
		
		ct.add(cmp);
		ct.doLayout();
	},
	
	removeComponent: function(id){
		var ct = Ext.getCmp(id);
		if(!ct) return;
		
		var oct = ct.ownerCt;
		
		oct.remove(ct);
		oct.doLayout();
	},
	replaceComponent : function(id, cmp){	
		var ct = Ext.getCmp(id);
		if(!ct) return;
		
		var find = function(cmt,cnt,ind) {
			if(cnt.items.get(ind) == undefined)
				return ind;
			if(cnt.items.get(ind) == cmt)
				return ind;
			else
				return find(cmt,cnt,ind + 1);
		}
		
		var oct = ct.ownerCt;
		
		//Find the index of the reference component
		var index = find(ct, ct.ownerCt, 0);
		
		oct.remove(index);
		oct.insert(index, cmp);
	
		oct.doLayout();	
	},
	enableComponent : function(id,enabled){
		var ct = Ext.getCmp(id);
		if(ct && ct.setDisabled) {
			ct.setDisabled(!enabled);
		}
	}
});

Ext.reg('itasks.ttc.interactive',itasks.ttc.InteractiveContainer);