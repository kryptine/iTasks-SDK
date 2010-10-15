Ext.ns('itasks.ttc');

itasks.ttc.FormContainer = Ext.extend(itasks.ttc.TTCBase, {

	initComponent : function() {
		this.cls = 'TTCFormContainer';	
		itasks.ttc.FormContainer.superclass.initComponent.apply(this,arguments);
	},
	buildComponents: function(data){
		this.interactionpanel = {
			xtype: 'panel',
			cls: 'TTCFormPanel',
			width: 720,
			layout: 'form',
			unstyled: true,
			autoScroll: true,
			items: data.content.form,
			buttons: data.content.buttons
		};
	},
	update: function(data) {
	
		if(data.updates) {
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
					case "TUISetValue":
						this.setComponentValue(update[1],update[2]);
						break;
					case "TUIReplaceMenu":
						this.replaceToolbar(update[1]);
						break;
					case "TUISetHint":
						this.setComponentHint(update[1],update[2]);
						break;
					case "TUISetError":
						this.setComponentError(update[1],update[2]);
						break;
				}
			}
			
			//cascade through the structure and update errors and hints
			//this.updateErrorsNHints(data.updates);
		} else {
			//Completely replace form
			this.rebuildComponents(data);
			this.replaceToolbar(data.content.tbar);
		}
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
	},
	
	setComponentValue : function(id,value){
		var ct = Ext.getCmp(id);
		
		if(!ct || !ct.setValue) return;
		//suspend events to prevent check-event for checkbox
		ct.suspendEvents();
		if (ct.xtype == "radio") {
			if(value == "true") {
				//first unset current element...
				var group = ct.findParentByType("radiogroup");
				var cur = group.getValue();
				if(cur.setValue)
					cur.setValue(false);
				//...then set new one
				ct.setValue(true);
			}			
		}else {
			ct.setValue(value);
		}
		ct.resumeEvents();					
	},
	setComponentHint: function (id, value) {
		var ct = Ext.getCmp(id);
		
		if(!ct || !ct.setHint) {
			console.log("can't set hint on item " + id);
			return;
		} else
			ct.setHint(value);
	},
	setComponentError: function (id, value) {
		var ct = Ext.getCmp(id);
		
		if(!ct || !ct.setError)
			return;
		else
			ct.setError(value);
	},
	replaceToolbar: function(newTb) {
		var tb = this.getTopToolbar();
		if(tb) {
			tb.removeAll();
			if(newTb)
				tb.add(newTb);
			this.setupToolbar(tb);
		}
	}
});

Ext.reg('itasks.ttc.form',itasks.ttc.FormContainer);