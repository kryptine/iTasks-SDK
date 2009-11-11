/**
* Tab panel which shows a task a user is working on
*/

Ext.ns("itasks");

itasks.WorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: null,
	properties: null,
	
	debug: false,
	initialized: false,
	
	initComponent: function() {
		Ext.apply(this, {
			title: "Loading task...",
			closable: true,
			autoDestroy: true,
			iconCls: "icon-task",
			
			url: "/handlers/work/tab",
			params: {_maintask: this.taskId, _debug: itasks.app.debug ? 1 : 0},
			
			layout: "anchor",
			deferredRender: false,
			items: [{
				xtype: "itasks.work-header",
				height: 25,
				anchor: "r"
			},{
				xtype: "tabpanel",
				anchor: "r -25",
				cls: "worktab-container",
				tabPosition: "bottom",
				layoutOnTabChange: true,
				activeTab: 0,
				items: [{
					title: "Task",
					iconCls: "icon-task",
					border: false,
					bodyStyle: "padding: 10px;",
					autoScroll: true	
				},{
					title: "Status",
					xtype: "itasks.work-status",
					iconCls: "icon-waiting",
					border: false,
					autoScroll: true
				}],
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
	
		this.addEvents("taskRedundant","taskDone","propertyChanged");
	
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
		if(data.content == "done" || data.content == "redundant") {
			var ct = this.getComponent(1).getComponent(0);
			
			if(ct.items && ct.items.length) {
				ct.remove(0);
			}
			switch(data.content) {
				case "done":	
					ct.add(new itasks.WorkMessagePanel({
						html: "This task is completed. Thank you."
					}));
					this.fireEvent("taskDone");
					break;
				case "redundant":
					ct.add(new itasks.WorkMessagePanel({
						html: "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort."
					}));
					this.fireEvent("taskRedundant");
					break;	
			}
			ct.doLayout();			
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
		this.updateStatus(data.properties);
	
		//Reset params, events and states
		this.params = { _maintask : this.taskId
					  , _debug: itasks.app.debug ? 1 : 0
					  }
	},
	updateTitle: function(subject) {
		this.setTitle(Ext.util.Format.ellipsis(subject.join(" - "),10));
	},
	updateContent: function(content) {
		
		var ct = this.getComponent(1).getComponent(0);

		if(this.initialized) {
			//Recursively update tab content
			var cur = ct.getComponent(0);
			
			if(cur.taskId == content.taskId && cur.xtype == content.xtype) {
				cur.update(content);
			} else {
				ct.remove(0,true);
				ct.add(content);
				ct.doLayout();
			}
		} else {
			//Build initial content
			ct.add(content);
			ct.doLayout();
			this.initialized = true;
		}	
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
			url:"/handlers/work/property",
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
	}
});

itasks.WorkHeaderPanel = Ext.extend(Ext.Panel, {
				
	initComponent: function() {
		Ext.apply(this, {
			deferredRender: false,
			html: "Loading..."
		});
		itasks.WorkHeaderPanel.superclass.initComponent.apply(this,arguments);
		
	},
	setContent: function(taskid, subject, properties) {		
			worktabStatus = function(progress){
				switch(progress){
					case null : return ""
					case "TPActive" : return "worktab-header-progress-active"
					case "TPStuck" : return "worktab-header-progress-stuck"
					case "TPWaiting": return "worktab-header-progress-waiting"
					case "TPReject": return "worktab-header-progress-reject"
				}
			}
			
			worktabBackground = function(priority){
				switch(priority) {
					case null : return "";
					case "LowPriority": return "worktab-header-low-priority"
					case "NormalPriority": return "worktab-header-normal-priority"
					case "HighPriority": return "worktab-header-high-priority"
				}		
			}
			
			var subject = subject.join(" &raquo; ") + (itasks.config.debug ? (" (" + taskid + ")") : "");
			
			this.body.update( String.format(
				'<div class="worktab-header {1}">'+
					'<div class="worktab-header-status {0}"></div><div class="worktab-header-separator"></div><div class="worktab-header-text">'+
						'<table><tr><th>Subject:</th><td>{2}</td><th>Delegated by:</th><td>{3}</td><th>Deadline:</th><td>{4}</td></table>'+
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

itasks.WorkMessagePanel = Ext.extend(Ext.Panel, {
	
	timeout: 5000,
	interval: 10,
	timepassed: 0,
	runner: null,
	
	initComponent: function() {	
		Ext.apply(this, {
			cls: "worktab-content",
			border: false,
			items: [{
				xtype: "panel",
				border: false,
				html: this.html
			},{
				xtype: "progress",
				style: "margin: 10px 0px 0px 0px;",
				value: 1.0,
				text: "This window will automatically close in " + (this.timeout / 1000) + " seconds"
			}],
			html: null
		});
		itasks.WorkMessagePanel.superclass.initComponent.apply(this,arguments);
		
		this.runner = {
			run: this.update,
			scope: this,
			interval: this.interval
		};
		
		Ext.TaskMgr.start(this.runner);
	},
	update: function() {
		if(this.timepassed >= this.timeout) {
			//Close the parent work panel
			var pt = this.findParentByType("itasks.work");
			if(pt.ownerCt)
				pt.ownerCt.remove(pt);
		} else {
			//Update progress
			this.timepassed += this.interval;
			
			var pb = this.getComponent(1);
		
			pb.updateText("This window will automatically close in " + Math.ceil((this.timeout - this.timepassed) / 1000) + " seconds");
			pb.updateProgress((this.timeout - this.timepassed) / this.timeout );
		}
	},
	onDestroy: function() {
		//Stop the taskrunner
		if(this.runner) {
			Ext.TaskMgr.stop(this.runner);
		}
		itasks.WorkMessagePanel.superclass.onDestroy.apply(this,arguments);
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
					"change" : function(ov,nv) {var wt = this.findParentByType("itasks.work"); wt.sendPropertyEvent(wt.properties.systemProps.processId,"progress",nv); }
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

itasks.TaskExtFormPanel = Ext.extend(Ext.form.FormPanel, {

	initComponent: function() {
		Ext.apply(this, {
			taskUpdates: {},
			border: false,
			url: "/handlers/work/tab",
			bodyStyle: "margin: 10px"
		});
		itasks.TaskExtFormPanel.superclass.initComponent.apply(this,arguments);
	},
	onRender: function() {
		itasks.TaskExtFormPanel.superclass.onRender.apply(this,arguments);
		this.attachTaskHandlers(this);
	},
	attachTaskHandlers: function(comp) {
	
		var changeTaskEvent = function () {
			var ct = this.findParentByType("itasks.task-ext-form");
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
				//console.log(Ext.encode(values));
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
			var ct = this.findParentByType("itasks.task-ext-form");
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
	},
	addUpdate: function(name, value) {
		this.taskUpdates[name] = value;
	},
	sendUpdates: function(delay) {
		if(delay) {
			new Ext.util.DelayedTask().delay(250,this.sendUpdates,this);
		} else {
			var wt = this.findParentByType("itasks.work");
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
							ct.setValue(update[2]);
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
							"change" : { fn: function(ov,nv) {this.findParentByType("itasks.work").sendPropertyEvent(this.properties.systemProps.processId,"user",nv);}
									   , scope: this }
						}
					},{
						xtype: "itasks.priority",
						fieldLabel: "Priority",
						value: this.properties.managerProps.priority, 
						listeners: {
							"change" : { fn: function(ov,nv) {this.findParentByType("itasks.work").sendPropertyEvent(this.properties.systemProps.processId,"priority",nv);}
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
		
		this.getComponent(0).body.update("Waiting for <i>" + Ext.util.Format.htmlEncode(data.properties.managerProps.subject) + "</i>");
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

//Combination of other tasks
itasks.TaskCombinationPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		Ext.apply(this, {
			border: false,
			layout: this.combination == "horizontal" ? "hbox" : null
		});

		//Set flex property for children of horizontal layouts
		if(this.combination == "horizontal") {
			for(var i = 0; i < this.items.length; i++) {
				this.items[i].flex = 1;
			}
		}
		itasks.TaskCombinationPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function (data) {
		//Check and update all child items
		var items = data.items;
		
		for(var i = 0; i < items.length; i++) {
			//Check if the next item already exists
			var j;
			for(j = i; j < this.items.length; j++) {
				if(this.items.get(j).taskId == items[i].taskId)
					break; //Found!
			}
			if(j < this.items.length) { //When found...
				//Remove the existing items between i and j
				for(var k = 0; k < (j - i); k ++) {
					this.remove(i);
				}
				//Update the existing item
				this.items.get(i).update(items[i]);
			} else {
				//Add the new item
				this.insert(i,items[i]);
			}
		}
		//Remove the trailing items
		var trailing = (this.items.length - items.length);
		for (var i = 0; i < trailing; i++) {
			this.remove(items.length);
		}
		
		//Update flex for horizontal layouts
		if(this.combination == "horizontal") {
			for(var i = 0; i < items.length; i++) {
				this.items.get(i).flex = 1;
			}
		}
		this.doLayout();
	}
});

Ext.reg("itasks.work",itasks.WorkPanel);
Ext.reg("itasks.work-header",itasks.WorkHeaderPanel);
Ext.reg("itasks.work-status",itasks.WorkStatusPanel);
Ext.reg("itasks.task-ext-form",itasks.TaskExtFormPanel);
Ext.reg("itasks.task-monitor",itasks.TaskMonitorPanel);
Ext.reg("itasks.task-waiting",itasks.TaskWaitingPanel);
Ext.reg("itasks.task-combination",itasks.TaskCombinationPanel);