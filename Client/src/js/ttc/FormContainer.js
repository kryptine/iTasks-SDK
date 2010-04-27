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
			items: data.content.form
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
		//attachTaskHandlers is moved to file 'TTCCommon.js'		
		itasks.ttc.common.attachTaskHandlers(this);
		
		var tb = this.getTopToolbar();
		this.setupToolbar(tb);
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
						var ct = Ext.getCmp(update[1]); 
						if(ct) {
							//Find the index of the reference component
							var find = function(cmt,cnt,ind) {
								if(cnt.items.get(ind) == undefined)
									return ind;
								if(cnt.items.get(ind) == cmt)
									return ind;
								else
									return find(cmt,cnt,ind + 1);
							}
														
							var index = find(ct, ct.ownerCt, 0) + 1;
							var newct = ct.ownerCt.insert(index, update[2]);
							
							ct.ownerCt.doLayout();
							ct.ownerCt.syncSize();
							ct.ownerCt.ownerCt.doLayout();
							
							itasks.ttc.common.attachTaskHandlers(newct);
						}
						break;
					case "TUIAddTo":
						var ct = Ext.getCmp(update[1]);
						if(ct){
							ct.add(update[2]);		
							ct.doLayout();
						
							itasks.ttc.common.attachTaskHandlers(ct);							
						}
						
						break;					
					case "TUIRemove":
						var ct = Ext.getCmp(update[1]);
						
						if(ct) {
							var oct = ct.ownerCt;
							
							oct.remove(update[1]);							
							oct.ownerCt.doLayout();
							oct.ownerCt.syncSize();
						}
						break;
					case "TUIReplace":
						var ct = Ext.getCmp(update[1]);
						if(ct) {
							var oct = ct.ownerCt;
							//Find the index of the reference component
							var find = function(cmt,cnt,ind) {
								if(cnt.items.get(ind) == undefined)
									return ind;
								if(cnt.items.get(ind) == cmt)
									return ind;
								else
									return find(cmt,cnt,ind + 1);
							}
							
							var index = find(ct, ct.ownerCt, 0);
							
							oct.remove(index);
							var newct = oct.insert(index, update[2]);
							
							oct.doLayout();
							oct.syncSize();
							//oct.ownerCt.doLayout();
							
							//this.attachTaskHandlers(newct);
							itasks.ttc.common.attachTaskHandlers(newct);
						}
						
						break;
					case "TUISetEnabled":
						var ct = Ext.getCmp(update[1]);
						if(ct && ct.setDisabled) {
							ct.setDisabled(!update[2]);
						}
						break;
					case "TUISetValue":
						var ct = Ext.getCmp(update[1]);
						
						if(ct && ct.setValue) {
							//suspend events to prevent check-event for checkbox
							ct.suspendEvents();
							if (ct.xtype == "radio") {
								if(update[2] == "true") {
									//first unset current element...
									var group = ct.findParentByType("radiogroup");
									var cur = group.getValue();
									if(cur.setValue)
										cur.setValue(false);
									//...then set new one
									ct.setValue(true);
								}			
							}else {
								ct.setValue(update[2]);
							}
							ct.resumeEvents();
						}				
						break;
					case "TUIReplaceMenu":
						this.replaceToolbar(update[1]);
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
			
			//Attach eventhandlers
			//this.attachTaskHandlers(this);
			itasks.ttc.common.attachTaskHandlers(this);
		}
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