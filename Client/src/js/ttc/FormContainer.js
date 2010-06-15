Ext.ns('itasks.ttc');

itasks.ttc.FormContainer = Ext.extend(Ext.Panel, {

	initComponent : function() {
		this.buildComponents(this);
		this.tbar = this.content.tbar;
		this.replaceItems = [];
		delete this.content;
				
		Ext.apply(this,
		{ layout: 'anchor'
		, taskUpdates : {}
		, url: itasks.config.serverUrl + '/work/tab'
		, items: [this.descpanel,this.panel]
		, unstyled: true
		, autoScroll: true
		, cls: 'FormContainer'
		});

		itasks.ttc.FormContainer.superclass.initComponent.apply(this,arguments);
	},
	
	buildComponents: function(data){
		data.content.form.buttons = data.content.buttons;
		
		this.panel = {
			xtype: 'itasks.ttc.form.panel',
			items: data.content.form,
			width: 720
		}
		
		this.descpanel = {
			xtype: 'itasks.ttc.common.description',
			cls: 'FormDescription',
			description: data.description,
			headerButton: this.headerButton,
			width: 720
		}
	},
	
	afterRender: function(){
		itasks.ttc.FormContainer.superclass.afterRender.call(this,arguments);	
		itasks.ttc.common.attachTaskHandlers(this);
		
		var tb = this.getTopToolbar();
		this.setupToolbar(tb);
		this.setupHotkeys(this.hotkeys);
	},
		
	addUpdate: function(name, value) {
		this.taskUpdates[name] = value;
	},
	
	sendUpdates: function(delay) {
		if(delay) {
			new Ext.util.DelayedTask().delay(250,this.sendUpdates,this);
		} else {
			var wt = this.findParentByType(itasks.WorkPanel);
			if(!wt) return;
			
			wt.sendTaskUpdates(this.taskId,this.taskUpdates);
		
			this.taskUpdates = {};
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
		
		this.setupHotkeys(data.hotkeys);
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
	},
	
	setupToolbar: function(tb) {
		tb.setVisible(tb.items.length > 0);
		
		var enabledPresent = false;
		for(var i = 0; i < tb.items.length; i++)
			if(!tb.get(i).disabled) {
				enabledPresent = true;
				break;
			}
		
		var cls = 'ToolbarNoEnabledItems';
		if(enabledPresent)
			tb.removeClass(cls);
		else {
			tb.removeClass(cls);
			tb.addClass(cls);
		}
		
		var checkGroupOnly = function(item) {
			if(item.disabled)
				return true;
				
			if(item.name) {
				return item.name == '_group';
			} else if (item.getXType() != 'menuseparator') {
				var children =  item.items || item.menu.items;
				for(var i = 0; i < children.length; i++) {
					if (!checkGroupOnly(children.get(i)))
						return false;
				}
			}
			return true;
		};
		
		var cls = 'ToolbarGroupActionsOnly';
		if(checkGroupOnly(tb)) {
			tb.removeClass(cls);
			tb.addClass(cls);
		} else {
			tb.removeClass(cls);
		}
	},
	
	setupHotkeys: function(hotkeys) {
		if (this.keyMap)
			this.keyMap.disable();
	
		if (hotkeys.length == 0)
			return;
		
		var conf = new Array();
		var form = this;
		for (i = 0; i < hotkeys.length; i++) {
			var h = hotkeys[i][1];
			conf[conf.length] = {
				key: h.keys,
				ctrl: h.ctrl,
				alt: h.alt,
				shift: h.shift,
				stopEvent: true,
				handler: function() {
					form.addUpdate('hotkey', this);
					form.sendUpdates();
				},
				scope: hotkeys[i][0]
			};
		}

		this.keyMap = new Ext.KeyMap(this.getEl(), conf);
	}
});

Ext.ns('itasks.ttc.form');

itasks.ttc.form.FormPanel = Ext.extend(Ext.Panel, {

	initComponent : function(){
	
		Ext.apply(this,
		{ layout: 'fit'
		, unstyled: true
		, cls: 'FormPanel'
		, width: 720
		});
		
		itasks.ttc.form.FormPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.ttc.form',itasks.ttc.FormContainer);
Ext.reg('itasks.ttc.form.panel', itasks.ttc.form.FormPanel);