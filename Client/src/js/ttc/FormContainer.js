Ext.ns('itasks.ttc');

itasks.ttc.FormContainer = Ext.extend(itasks.ttc.InteractionBase, {

	initComponent : function() {
		this.cls = 'FormContainer';
		
		itasks.ttc.FormContainer.superclass.initComponent.apply(this,arguments);
	},
	
	buildComponents: function(data){
		this.panel = new itasks.ttc.form.FormPanel({
			xtype: 'itasks.ttc.form.panel',
			items: data.content.form,
			buttons: data.content.buttons,
			width: 720
		});
		
		this.descpanel = {
			xtype: 'itasks.ttc.common.description',
			cls: 'FormDescription',
			description: data.description,
			headerButton: this.headerButton,
			width: 720
		}
	},
	
	update: function(data) {
		if(data.updates) {
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
					case "TUISetError":
						var ct = Ext.getCmp(update[1]);
						if(ct && ct.setError) ct.setError(update[2]);					
					break;
					case "TUISetHint":
						var ct = Ext.getCmp(update[1]);
						if(ct && ct.setHint) ct.setHint(update[2]);
					break;
				}
			}			
		} else {			
			//Completely replace form
			this.taskId = data.taskId;
			this.removeAll();
			this.buildComponents(data);
			
			this.add(this.descpanel);
			this.add(this.panel);
			
			this.doLayout();
			
			this.replaceToolbar(data.content.tbar);
			itasks.ttc.common.attachTaskHandlers(this);
		}
		
		//this.setupHotkeys(data.hotkeys);
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
		
		//ct.ownerCt.doLayout();
		//ct.ownerCt.syncSize();
		ct.ownerCt.ownerCt.doLayout();
		
		itasks.ttc.common.attachTaskHandlers(newct);
	},
	
	addComponentTo : function(parent, cmp){
		var ct = Ext.getCmp(parent);
		if(!ct) return;
		
		ct.add(cmp);
		ct.doLayout();
		
		itasks.ttc.common.attachTaskHandlers(ct);		
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
		//oct.doLayout();
		
		var newct = oct.insert(index, cmp);
		
		var f = function(){ this.doLayout(); itasks.ttc.common.attachTaskHandlers(this); };
		f.defer(50,oct);
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
		
	replaceToolbar: function(newTb) {
		var tb = this.getTopToolbar();
		tb.removeAll();
		if(newTb)
			tb.add(newTb);
		this.setupToolbar(tb);
	}
});

Ext.ns('itasks.ttc.form');

itasks.ttc.form.FormPanel = Ext.extend(Ext.Panel, {

	initComponent : function(){
	
		Ext.apply(this,
		{ unstyled: true
		, cls: 'FormPanel'
		, width: 720
		, layout: 'auto'
		, autoScroll: true
		});
		
		itasks.ttc.form.FormPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.ttc.form',itasks.ttc.FormContainer);
Ext.reg('itasks.ttc.form.panel', itasks.ttc.form.FormPanel);