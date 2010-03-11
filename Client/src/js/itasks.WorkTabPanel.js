/**
* Tab panel which shows a task a user is working on
*/

Ext.ns("itasks");

itasks.WorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: null,
	properties: null,
	
	debug: false,
	activeSubtaskId: 0,
	
	initComponent: function() {
		Ext.apply(this, {
			title: "Loading task...",
			closable: true,
			autoDestroy: true,
			iconCls: "icon-task",
			url: itasks.config.serverUrl + "/work/tab",
			params: {_maintask: this.taskId, _debug: itasks.app.debug ? 1 : 0},
			layout: "border",
			items: [{
				xtype: "itasks.work-header",
				region: "north",
				height: 25
			},{
				xtype: "tabpanel",
				region: "center",
				ctCls: "worktab-container",
				tabPosition: "bottom",
				layoutOnTabChange: true,
				tbar: [{
						text: "Refresh task",
						iconCls: "x-tbar-loading",
						listeners: {
							click: {
								scope: this,
								fn: function (btn) {
									this.refresh();
								}
							}
						}
				}]
			}]
		});
		
		itasks.WorkPanel.superclass.initComponent.apply(this, arguments);
	
		this.addEvents("taskRedundant","taskDone","propertyChanged","afterUpdateContent");
	
		//Attach tab change handler
		this.getComponent(1).on("tabchange",function(ct,tab){
			if(tab && tab.subtaskId){
				this.activeSubtaskId = tab.subtaskId;
			}
		},this);
		
		this.on("afterUpdateContent",function(){
			var me = this;
			(function(){me.doLayout(false,true)}).defer(100);
		});
		
		//Attach event handlers for the loading indicator
		this.on("remoteCallStart",function() {
			this.getComponent(0).setBusy(true);
		},this);
		this.on("remoteCallEnd",function() {
			this.getComponent(0).setBusy(false);
		},this);
	},
	update: function(data) {

		//Check if the task is finished or became redundant
		if(data.content[0] == "done" || data.content[0] == "redundant") {
			var ct = this.getComponent(1).getComponent(0);
			
			if(ct.items && ct.items.length) {
				ct.remove(0);
			}
			switch(data.content[0]) {
				case "done":	
					ct.add(new Ext.Panel({
						html: "This task is completed. Thank you."
					}));
					this.fireEvent("taskDone");
					break;
				case "redundant":
					ct.add(new Ext.Panel({
						html: "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort."
					}));
					this.fireEvent("taskRedundant");
					break;	
			}
			ct.doLayout();			
			
			var tp = this.findParentByType("itasks.worktabs");
			var tab = this;
			this.getEl().fadeOut(
				{ scope: this
				, duration: .5
				, useDisplay: true
				, callback: function()
					{ 
						tp.remove(tab);
					}
				}
			);
			
			return;
		}
		//Update properties
		this.properties = data.properties;
		//Update header
		this.getComponent(0).setContent(this.taskId, data.subject, data.properties);
		//Update title
		this.updateTitle(data.subject);
		//Update content
		this.updateContent(data.content);
		//Update status
		//this.updateStatus(data.properties);
	
		//Reset params, events and states
		this.params = 
		{ _maintask : this.taskId
		, _debug: itasks.app.debug ? 1 : 0
		}
	},
	updateTitle: function(subject) {
		this.setTitle(Ext.util.Format.ellipsis(subject.join(" - "),10));
	},
	updateTabContent: function(tab,content){

		if(tab.initialized){
			var cur = tab.getComponent(0);
			if(cur.taskId == content.taskId && cur.xtype == content.xtype){
				cur.update(content);
			}else{
				tab.remove(0,true);
				tab.add(content);
			}
		}else{
			tab.add(content);
			tab.initialized = true;
		}
		
		tab.taskId = content.taskId;
		tab.subtaskId = content.subtaskId;
		
		if(content.xtype=="itasks.task-parallel"){
			tab.setIconClass("icon-overview");
		}else{
			tab.setIconClass("icon-task");
		}
		
		if(content.subtaskId){
			tab.setTitle('Subtask '+content.subtaskId);
		}else{
			tab.setTitle('Task');
		}
	},
	updateContent: function(content) {
		var tabpanel = this.getComponent(1);
		
		//filter the content array
		content = content.filter(function (val) {
			if(val == "done" || val == "redundant") return false;
			else return true;
		});

		for(var i=0; i<content.length; i++){
			for(var j=i; j<tabpanel.items.length;j++){
				if(content[i].taskId == tabpanel.items.get(j).taskId) break;
			}
					
			for(var k=0; k < (j-i); k++){
				tabpanel.remove(i,true);
			}
			
			if(i<tabpanel.items.length){
				this.updateTabContent(tabpanel.items.get(i),content[i]);
			}else{
				var tab = new itasks.WorkPanelTab();
				this.updateTabContent(tab,content[i]);
				tabpanel.insert(j,tab);
			}			
		}
		
		var trailing = tabpanel.items.length - content.length;
		
		for(var i=0; i<trailing; i++){
			tabpanel.remove(tabpanel.items.length-1,true);
		}
				
		var t = 0;
		for(var i=0; i<content.length; i++){
			if(content[i].subtaskId == this.activeSubtaskId){
				t=i;
				break;
			}
		}			
		tabpanel.setActiveTab(t);	
		
		this.fireEvent("afterUpdateContent");
	},
	updateStatus: function(properties) {
		this.getComponent(1).getComponent(1).update(properties);
	},
	sendTaskUpdates: function(target,updates) {
		//Add task updates to params
		Ext.apply(this.params, updates);
			
		//Set target and state
		this.params["_targettask"]	= target;
		this.refresh();
	},
	sendPropertyEvent: function(process,name,value) {
		//Ugly side-effect event handler
		this.getComponent(0).setBusy(true);
		
		Ext.Ajax.request({
			url: itasks.config.serverUrl + "/work/property",
			method: "GET",
			params: {_session : itasks.app.session, process : process, property: name, value: value },
			callback: function(el,success,response,options) {
				this.getComponent(0).setBusy(false);
				this.fireEvent("propertyChanged");
				if(name == "user" || name == "progress") //HACK: Fix with proper property events
					this.refresh();
			},
			scope: this
		});
	},
	switchToSubtask: function(taskId){
		var t = 0;
		var tabpanel = this.getComponent(1);
		
		for(var i=0; i<tabpanel.items.length; i++){
			var c = tabpanel.items.get(i);
			
			if(c.taskId == taskId){
				t=i;
			}
		}
		
		tabpanel.setActiveTab(t);
	}
});

itasks.WorkHeaderPanel = Ext.extend(Ext.Panel, {
				
	initComponent: function() {
		Ext.apply(this, {
			deferredRender: false,
			html: "Loading...",
			baseCls: "worktab-header-normal-priority"
		});
		itasks.WorkHeaderPanel.superclass.initComponent.apply(this,arguments);
		
	},
	setContent: function(taskid, subject, properties) {		
			worktabStatus = function(progress){
				switch(progress){
					case null : return "";
					case "TPActive" : return "worktab-header-progress-active";
					case "TPStuck" : return "worktab-header-progress-stuck";
					case "TPWaiting": return "worktab-header-progress-waiting";
					case "TPReject": return "worktab-header-progress-reject";
				}
			}
			
			worktabBackground = function(priority){
				switch(priority) {
					case null : return "";
					case "LowPriority":
					case "NormalPriority": return "worktab-header-normal-priority";
					case "HighPriority": return "worktab-header-high-priority";
				}		
			}
			
			var subject = subject.join(" &raquo; ") + (itasks.config.debug ? (" (" + taskid + ")") : "");
			
			this.body.update( String.format(
				'<div class="worktab-header {1}">'+
					'<div class="worktab-header-status {0}"></div><div class="worktab-header-separator"></div><div class="worktab-header-text">'+
						'<table><tr><th>Subject:</th><td>{2}</td><th>Managed by:</th><td>{3}</td><th>Deadline:</th><td>{4}</td></table>'+
					'</div>'+
				'</div>'+
				'<div class="worktab-header-indicator">'
				, worktabStatus(properties.workerProps.progress),worktabBackground(properties.managerProps.priority),subject, properties.systemProps.manager[1]
				, itasks.util.formatDeadline(properties.managerProps.deadline)
				));
	},
	setBusy: function(busy) {
		var indicator = this.getEl().child(".worktab-header-indicator");
		if(indicator)
			indicator.setVisible(busy);
	}	
});

itasks.WorkStatusPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		Ext.apply(this, {
			layout: "form",
			cls: "worktab-content",
			defaultType: "staticfield",
			items: [{
				xtype: "itasks.progress",
				name: "progress",
				fieldLabel: "Progress",
				format: itasks.util.formatProgress,
				listeners: {
					"change" : function(ov,nv) {var wt = this.findParentByType(itasks.WorkPanel); wt.sendPropertyEvent(wt.properties.systemProps.processId,"progress",nv); }
				}
			},{
				name: "priority",
				fieldLabel: "Priority",
				format: itasks.util.formatPriority
			},{
				name: "issuedAt",
				fieldLabel: "Issued at",
				format: itasks.util.formatDate
			},{
				name: "firstEvent",
				fieldLabel: "First worked on",
				format: itasks.util.formatStartDate
			},{
				name: "latestEvent",
				fieldLabel: "Last worked on",
				format: itasks.util.formatStartDate	
			}]
		});
		itasks.WorkStatusPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function (p) {
		var props = [p.workerProps.progress,p.managerProps.priority,p.systemProps.issuedAt,p.systemProps.firstEvent,p.systemProps.latestEvent];
		this.items.each(function(cmt,i){ cmt.setValue(props[i]); });
	}
});

itasks.TaskFormPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		Ext.apply(this, {
			taskUpdates: {},
			unstyled: true,
			layout: "fit",
			url: itasks.config.serverUrl+"/work/tab",
			bodyStyle: "padding: 10px"
		});
		itasks.TaskFormPanel.superclass.initComponent.apply(this,arguments);
	},
	afterRender: function() {
		itasks.TaskFormPanel.superclass.afterRender.apply(this,arguments);
		this.attachTaskHandlers(this);
		var tb = this.getTopToolbar();
		if(tb)
			this.attachTaskHandlers(tb);
	},
	attachTaskHandlers: function(comp) {
	
		// Scary hack! Don't look below!
		new Ext.util.DelayedTask().delay(100,this.attachDocumentLinkInformation,this);
		// End of scary hack
		
		var changeTaskEvent = function () {
			var ct = this.findParentByType(itasks.TaskFormPanel);
			if(!ct)
				return;

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
			
			var ct = this.findParentByType(itasks.TaskFormPanel);
			if(!ct)
				return;
						
			ct.addUpdate(this.name, this.value);
			ct.sendUpdates();
			
		};
		switch(comp.getXType()) {
				case "textfield":
				case "textarea":
				case "numberfield":
				case "datefield":
				case "timefield":
				case "radiogroup":
					comp.on("change",changeTaskEvent);
					break;
				case "checkbox":
					comp.on("check",changeTaskEvent);
					break;
				case "checkboxgroup":
					comp.on("change",changeTaskEvent);
					break;
				case "combo":
					comp.on("select",changeTaskEvent);
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
			if(!wt)
				return;
			
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
			this.add(data.items[0]);
			this.doLayout();
			//Attach eventhandlers
			this.attachTaskHandlers(this);
		}
	}
});

itasks.TaskMonitorPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		Ext.apply(this,{
			defaults:{
				unstyled: true
			},
			unstyled: true,
			bodyStyle: 'padding: 10px'
		});
		itasks.TaskMonitorPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function(data) {
		this.el.update(data.html);
	}
});

//Waiting for main task panel
itasks.TaskWaitingPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {	
			
		Ext.apply(this, {
			cls: "worktab-content",
			border: false,
			hideBorders: true,
			items: [{
					html: "Waiting for <i>" +  this.properties.managerProps.subject + "</i>",
					style: "margin: 0px 0px 20px 0px;"			
				},{
					xtype: "panel",
					layout: "form",
					defaultType: "staticfield",
					items: [{
						xtype: "itasks.user",
						fieldLabel: "Assigned to",
						value: this.properties.managerProps.worker[1],
						listeners: {
							"change" : { fn: function(ov,nv) {this.findParentByType(itasks.WorkPanel).sendPropertyEvent(this.properties.systemProps.processId,"user",nv);}
									   , scope: this }
						}
					},{
						xtype: "itasks.priority",
						fieldLabel: "Priority",
						value: this.properties.managerProps.priority, 
						listeners: {
							"change" : { fn: function(ov,nv) {this.findParentByType(itasks.WorkPanel).sendPropertyEvent(this.properties.systemProps.processId,"priority",nv);}
									   , scope: this }
						}
					},{
						fieldLabel: "Progress",
						format: itasks.util.formatProgress,
						value: this.properties.workerProps.progress
					},{
						fieldLabel: "Issued at",
						format: itasks.util.formatDate,
						value: this.properties.systemProps.issuedAt
					},{
						fieldLabel: "First worked on",
						format: itasks.util.formatStartDate,
						value: this.properties.systemProps.firstEvent
					},{
						fieldLabel: "Last worked on",
						format: itasks.util.formatStartDate,
						value: this.properties.systemProps.latestEvent
					}]
				}]
		});
		itasks.TaskWaitingPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function (data) {
		this.properties = data.properties;

		var p = data.properties;
		var props = [p.managerProps.worker[1],p.managerProps.priority,p.workerProps.progress,p.systemProps.issuedAt,p.systemProps.firstEvent,p.systemProps.latestEvent];
				
		if(this.getComponent(0).body) this.getComponent(0).body.update("Waiting for <i>" + Ext.util.Format.htmlEncode(data.properties.managerProps.subject) + "</i>");
		this.getComponent(1).items.each(function(cmt,i){ cmt.setValue(props[i]); });
	}
});


itasks.form.UserField = Ext.extend(itasks.form.InlineField, {
	format: function(value, field) { return (field.label != "") ? field.label : value;},
	field: {
		xtype: "combo",
		value: this.value,
		label: "",
		store: new Ext.data.JsonStore({
			root: "users",
			totalProperty: "total",
			fields: ["userId","displayName"],
			url: "/handlers/data/users"
		}),
		displayField: "displayName",
		valueField: "userId",
		triggerAction: "all",
		editable: false,
		forceSelection: true,
		listeners: {
			"select" : function(cmt,rec,ind) {cmt.label = rec.get("displayName");},
			"beforequery" : function(e) {e.combo.store.baseParams["_session"] = itasks.app.session;}
		}
	}
});

itasks.form.PriorityField = Ext.extend(itasks.form.InlineField, {
	format: itasks.util.formatPriority,
	field: {
		xtype: "combo",
		value: this.value,
		store: [["HighPriority","High"],["NormalPriority","Normal"],["LowPriority","Low"]],
		editable: false,
		triggerAction: "all",
		forceSelection: true
	}
});

itasks.form.ProgressField = Ext.extend(itasks.form.InlineField, {
	format: itasks.util.formatProgress,
	field: {
		xtype: "combo",
		value: this.value,
		store: [["TPActive","Active"],["TPStuck","Stuck"],["TPWaiting","Waiting"],["TPReject","Reject"]],
		editable: false,
		triggerAction: "all",
		forceSelection: true
	}
});

Ext.reg("itasks.user",itasks.form.UserField);
Ext.reg("itasks.priority",itasks.form.PriorityField);
Ext.reg("itasks.progress",itasks.form.ProgressField);

itasks.ParallelPanel = Ext.extend(Ext.Panel,{

	initComponent: function(){
		var store = new Ext.data.JsonStore({
			autoDestroy: true,
			root: 'subtasks',
			fields: [{name: 'finished', type: 'bool'},'taskId', 'subject', 'delegatedTo', 'progress','subtaskId','description']
		});

		var col = new Ext.grid.ColumnModel({
			defaults:{
				menuDisabled: true,
				sortable: true
			},
			columns: [
				{header: 'Done',     			dataIndex: 'finished', renderer: this.renderFinished, width: 36, resizable: false},
				{header: 'Nr.',			  		dataIndex: 'subtaskId', width: 75, renderer: this.renderId},
				{header: 'Subject',		    dataIndex: 'subject', width: 200},
				{header: 'Task Id', 			dataIndex: 'taskId', hidden: itasks.app.debug, width: 120},
				{header: 'Delegated To', dataIndex: 'delegatedTo', width: 120},
				{header: 'Description',	dataIndex: 'description', width: 400}
			]
		});

		var grid = new Ext.grid.GridPanel({
			store : store,
			disableSelection: true,
			colModel: col,	
			width: '100%',
			height: 250,
		});
		
		Ext.apply(this,{
			unstyled: true,
			bodyStyle: 'padding: 10px',
			items: [
				{ xtype: 'panel'
				, html: this.label
				, bodyCssClass: 'task-description'
				}
				, grid			
			]
		});
		
		grid.on("rowdblclick",function(grid,row,e){
			var store = grid.store;
			var rec = grid.store.getAt(row);
			var taskId = rec.data.taskId;
			
			var ct = grid.findParentByType(itasks.WorkPanel);
			ct.switchToSubtask(taskId);			
		});
		
		itasks.ParallelPanel.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(arguments){
		this.updateStore(this.subtaskInfo);
		itasks.ParallelPanel.superclass.afterRender.call(this,arguments);
	},
	
	update: function(content){
		this.updateStore(content.subtaskInfo);
	},
	
	updateStore: function(records){
		var grid = this.getComponent(1);
		var store = grid.store;
		store.loadData({subtasks: records},false);
	},
	
	renderFinished: function(val,metadata,rec,row,col,store){
		if(val == false){
			return '<div style="text-align: center"><img src="skins/default/img/icons/hourglass.png" /></div>'
		}else{
			return '<div style="text-align: center"><img src="skins/default/img/icons/tick.png" /></div>'
		}
	},
	
	renderId: function(val,metadata,rec,row,col,store){
		var split = val.split(".");
		render = '<img style="width: '+(split.length-1)*5+'px" src="'+Ext.BLANK_IMAGE_URL+'"></span>';		
		render += val;
		return render;
	}
});

itasks.WorkPanelTab = Ext.extend(Ext.Panel,{
	
	initialized: false,
	
	initComponent: function(){
		
		Ext.apply(this,{
			title: "Task",
			iconCls: "icon-task",
			unstyled: true,
			autoScroll: true,
			layout:'anchor',
			defaults:{
				anchor: 'r-'+itasks.app.scrollbarWidth
			}
		});

		itasks.WorkPanelTab.superclass.initComponent.apply(this,arguments);
	}
});
Ext.reg("itasks.work",itasks.WorkPanel);
Ext.reg("itasks.work-tab",itasks.WorkPanelTab);
Ext.reg("itasks.work-header",itasks.WorkHeaderPanel);
Ext.reg("itasks.work-status",itasks.WorkStatusPanel);
Ext.reg("itasks.task-form",itasks.TaskFormPanel);
Ext.reg("itasks.task-monitor",itasks.TaskMonitorPanel);
Ext.reg("itasks.task-waiting",itasks.TaskWaitingPanel);
Ext.reg("itasks.task-parallel",itasks.ParallelPanel);