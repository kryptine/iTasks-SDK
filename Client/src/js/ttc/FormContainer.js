Ext.ns('itasks.ttc');

itasks.ttc.FormContainer = Ext.extend(Ext.Panel, {

	initComponent : function() {
		
		this.buildComponents(this);
		this.tbar = this.content.tbar;
		
		delete this.content;
		delete this.description;
		
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
			xtype: 'itasks.ttc.form.description',
			html: data.description
		}
	},
	
	afterRender: function(){
		itasks.ttc.FormContainer.superclass.afterRender.call(this,arguments);
		this.attachTaskHandlers(this);
		
		var tb = this.getTopToolbar();
		if(tb) tb.setVisible(tb.items.length > 0);
	},
	
	attachTaskHandlers: function(comp) {
	
		// Scary hack! Don't look below!
		new Ext.util.DelayedTask().delay(100,this.attachDocumentLinkInformation,this);
		// End of scary hack
		
		var changeTaskEvent = function () {
			var ct = this.findParentByType(itasks.ttc.FormContainer);
			if(!ct) return;
			
			//Helper function to get the value of a checkbox group
			function checkboxValues(boxes) {
				var values = [];
				var num = boxes.length;
				for(var i = 0; i < num; i++) {
					values[values.length] = boxes[i].value;
				}
				return Ext.encode(values);
			}
			
			var value;
			switch(this.xtype) {
				
				case "radiogroup": value = this.getValue().value; break;
				case "checkboxgroup": value = checkboxValues(arguments[1]); break;
				case "datefield": value = this.getRawValue(); break;
				default: value = this.getValue();
			}
			ct.addUpdate(this.name, value);
			ct.sendUpdates(true);
		};
		var clickTaskEvent = function () {
			if(this.clickCB) this.clickCB(this);
			
			var ct = this.findParentByType(itasks.ttc.FormContainer);
			if(!ct) return;
						
			ct.addUpdate(this.name, this.value);
			ct.sendUpdates();
			
		};
		
		switch(comp.getXType()) {
				case "textfield":
				case "itasks.tui.String":
				case "itasks.tui.Char":
				case "itasks.tui.Int":
				case "itasks.tui.Real":
				case "itasks.tui.Note":
				case "itasks.tui.Date":
				case "itasks.tui.Time":
				case "itasks.tui.Username":
				case "itasks.tui.Currency":
				case "textarea":
				case "numberfield":
				case "datefield":
				case "timefield":
				case "radiogroup":
					comp.on("change",changeTaskEvent);
					break;
				case "checkbox":
				case "itasks.tui.Bool":
					comp.on("check",changeTaskEvent);
					break;
				case "checkboxgroup":
					comp.on("change",changeTaskEvent);
					break;
				case "combo":
				case "itasks.userfield":
					comp.on("select",changeTaskEvent);
					break;
				case "button":
				case "menuitem":
					if(comp.name)
						comp.on("click",clickTaskEvent);
					break;
		}
		
		if(comp.buttons) {
			var num = comp.buttons.length;
			for(var i = 0; i < num; i++) {
				comp.buttons[i].on("click",clickTaskEvent);
			}
		}
		//attach recursively
		if(comp.items && comp.items.each)
			comp.items.each(this.attachTaskHandlers, this);
		if(comp.menu)
			this.attachTaskHandlers(comp.menu);
		if(comp.topToolbar)
			this.attachTaskHandlers(comp.topToolbar);
	},
	
	attachDocumentLinkInformation: function() {
		
		var links   = Ext.query("a[name=x-form-document-link]");
		var plinks = Ext.query("a[name=x-form-document-preview-link]");
		
		for(var x=0; x < links.length; x++){
			var link = links[x];
			
			if(link.pathname.indexOf('/') != 0){
				link.pathname = itasks.config.serverUrl+'/'+link.pathname;
			}else{
				link.pathname = itasks.config.serverUrl+link.pathname;
			}
			link.href = Ext.urlAppend(link.href,'_session='+itasks.app.session);
			link.name = "";
			
			for(var y=0; y < plinks.length; y++){				
				if(plinks[y].id == link.id){
					var plink = plinks[y];		

					plink.href="javascript:itasks.preview('"+link.href.replace( 'download','preview')+"')";
					plink.name = "";
				}
			}
		}
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
							
							this.attachTaskHandlers(newct);
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
							oct.ownerCt.doLayout();
							
							this.attachTaskHandlers(newct);
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
							} else {
								ct.setValue(update[2]);
							}
							ct.resumeEvents();
						}				
						break;
				}
			}
			
		} else {			
			//Completely replace form
			this.removeAll();
			this.buildComponents(data);
			
			this.add(this.descpanel);
			this.add(this.panel);
			
			this.doLayout();
			
			//Replace toolbar
			var tb = this.getTopToolbar();
			tb.removeAll();
			if(data.content.tbar)
				tb.add(data.content.tbar);
			tb.setVisible(tb.items.length > 0);
			
			//Attach eventhandlers
			this.attachTaskHandlers(this);
		}
	}	
});

Ext.ns('itasks.ttc.form');

itasks.ttc.form.FormDescription = Ext.extend(Ext.Panel,{
	initComponent : function(){
		Ext.apply(this,{		
			cls: 'task-description FormDescription',
			unstyled: true,
			width:700
		});
		
		itasks.ttc.form.FormDescription.superclass.initComponent.apply(this,arguments);
	}
});

itasks.ttc.form.FormPanel = Ext.extend(Ext.Panel, {

	initComponent : function(){
		Ext.apply(this,
		{ layout: 'fit'
		, width: 700
		, unstyled: true
		, cls: 'FormPanel'
		});
		
		itasks.ttc.form.FormPanel.superclass.initComponent.apply(this,arguments);
	}

});

Ext.reg('itasks.ttc.form',itasks.ttc.FormContainer);
Ext.reg('itasks.ttc.form.panel', itasks.ttc.form.FormPanel);
Ext.reg('itasks.ttc.form.description', itasks.ttc.form.FormDescription);

/*
itasks.ttc.FormContainer = Ext.extend(Ext.Panel,{

	initComponent: function(){
		if(this.buttons) {
			this.content.buttons = this.buttons;
			delete this.buttons;
		}
		
		Ext.apply(this, 
		{ layout: 'fit'
		, taskUpdates: {}
		, url: itasks.config.serverUrl + '/work/tab'
		, bodyStyle: 'padding: 10px'
		, autoScroll: true
		, items: [this.content]
		});
		
		itasks.ttc.FormContainer.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.ttc.FormContainer.superclass.afterRender.apply(this,arguments);
		this.attachTaskHandlers(this);
		
		var tb = this.getTopToolbar();
		if(tb) tb.setVisible(tb.items.length > 0);
	},
	
	attachTaskHandlers: function(comp) {
	
		// Scary hack! Don't look below!
		new Ext.util.DelayedTask().delay(100,this.attachDocumentLinkInformation,this);
		// End of scary hack
		
		var changeTaskEvent = function () {
			var ct = this.findParentByType(itasks.ttc.FormContainer);
			if(!ct) return;
			
			//Helper function to get the value of a checkbox group
			function checkboxValues(boxes) {
				var values = [];
				var num = boxes.length;
				for(var i = 0; i < num; i++) {
					values[values.length] = boxes[i].value;
				}
				return Ext.encode(values);
			}
			
			var value;
			switch(this.xtype) {
				
				case "radiogroup": value = this.getValue().value; break;
				case "checkboxgroup": value = checkboxValues(arguments[1]); break;
				case "datefield": value = this.getRawValue(); break;
				default: value = this.getValue();
			}
			ct.addUpdate(this.name, value);
			ct.sendUpdates(true);
		};
		var clickTaskEvent = function () {
			if(this.clickCB) this.clickCB(this);
			
			var ct = this.findParentByType(itasks.ttc.FormContainer);
			if(!ct) return;
						
			ct.addUpdate(this.name, this.value);
			ct.sendUpdates();
			
		};
		
		switch(comp.getXType()) {
				case "textfield":
				case "itasks.tui.String":
				case "itasks.tui.Char":
				case "itasks.tui.Int":
				case "itasks.tui.Real":
				case "itasks.tui.Note":
				case "itasks.tui.Date":
				case "itasks.tui.Time":
				case "itasks.tui.Username":
				case "itasks.tui.Currency":
				case "textarea":
				case "numberfield":
				case "datefield":
				case "timefield":
				case "radiogroup":
					comp.on("change",changeTaskEvent);
					break;
				case "checkbox":
				case "itasks.tui.Bool":
					comp.on("check",changeTaskEvent);
					break;
				case "checkboxgroup":
					comp.on("change",changeTaskEvent);
					break;
				case "combo":
				case "itasks.userfield":
					comp.on("select",changeTaskEvent);
					break;
				case "button":
				case "menuitem":
					if(comp.name)
						comp.on("click",clickTaskEvent);
					break;
		}
		
		if(comp.buttons) {
			var num = comp.buttons.length;
			for(var i = 0; i < num; i++) {
				comp.buttons[i].on("click",clickTaskEvent);
			}
		}
		//attach recursively
		if(comp.items && comp.items.each)
			comp.items.each(this.attachTaskHandlers, this);
		if(comp.menu)
			this.attachTaskHandlers(comp.menu);
		if(comp.topToolbar)
			this.attachTaskHandlers(comp.topToolbar);
	},
	
	attachDocumentLinkInformation: function() {
		
		var links   = Ext.query("a[name=x-form-document-link]");
		var plinks = Ext.query("a[name=x-form-document-preview-link]");
		
		for(var x=0; x < links.length; x++){
			var link = links[x];
			
			if(link.pathname.indexOf('/') != 0){
				link.pathname = itasks.config.serverUrl+'/'+link.pathname;
			}else{
				link.pathname = itasks.config.serverUrl+link.pathname;
			}
			link.href = Ext.urlAppend(link.href,'_session='+itasks.app.session);
			link.name = "";
			
			for(var y=0; y < plinks.length; y++){				
				if(plinks[y].id == link.id){
					var plink = plinks[y];		

					plink.href="javascript:itasks.preview('"+link.href.replace( 'download','preview')+"')";
					plink.name = "";
				}
			}
		}
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
							
							this.attachTaskHandlers(newct);
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
							oct.ownerCt.doLayout();
							
							this.attachTaskHandlers(newct);
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
							} else {
								ct.setValue(update[2]);
							}
							ct.resumeEvents();
						}				
						break;
				}
			}
			
		} else {			
			//Completely replace form
		
			this.removeAll();
			data.content.buttons = data.buttons;
			this.add(data.content);
			this.doLayout();
			
			//Replace toolbar
			var tb = this.getTopToolbar();
			tb.removeAll();
			tb.add(data.tbar);
			tb.setVisible(tb.items.length > 0);
			
			//Attach eventhandlers
			this.attachTaskHandlers(this);
		}
	}
});
*/
