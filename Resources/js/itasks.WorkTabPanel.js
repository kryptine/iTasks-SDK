/**
* Tab panel which shows a task a user is working on
*/

Ext.ns("itasks");

itasks.WorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: null,
	sessionId: null,
	application: null,
	properties: null,
	
	debug: false,
	initialized: false,
	
	initComponent: function() {
		Ext.apply(this, {
			title: "Loading task...",
			closable: true,
			autoDestroy: true,
			iconCls: "icon-editTask",
			
			url: "/handlers/work/tab",
			params: {_maintask: this.taskId, _debug: this.debug ? 1 : 0},
			
			layout: "anchor",
			deferredRender: false,
			items: [{
				xtype: "itasks.work-header",
				height: 50,
				anchor: "r"
			},{
				xtype: "tabpanel",
				anchor: "r -50",
				cls: "worktab-container",
				tabPosition: "bottom",
				layoutOnTabChange: true,
				activeTab: 0,
				items: [{
					title: "Task",
					iconCls: "icon-editTask",
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
		//Update debug tab
		this.updateDebug(data.debug);
		//Update content
		this.updateContent(data.content);
		//Update status
		this.updateStatus(data.properties);
	
		//Reset params, events and states
		this.params = { _maintask : this.taskId
					  , _debug: this.debug ? 1 : 0
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
	updateDebug: function(debug) {
		if(debug != null) {
			if(this.getComponent(1).items.length > 2) {
				if(this.getComponent(1).getComponent(2).rendered) {
					this.updateDebugTab(debug);
				} else {
					this.removeDebugTab(debug);
					this.addDebugTab(debug);
				}
			} else {
				this.addDebugTab(debug);
			}
		} else {
			if(this.getComponent(1).items.length > 2)
				this.removeDebugTab();
		}
	},
	addDebugTab: function(debug) {
		this.getComponent(1).add({
			title: "Debug",
			iconCls: "icon-debug",
			bodyStyle: "padding: 10px",
			autoScroll: true,
			items: [{
				xtype: "panel",
				title: "Task tree",
				collapsible: true,
				html: debug.tasktree
			},{
				xtype: "panel",
				title: "Task states",
				collapsible: true,
				html: debug.states
			},{
				xtype: "panel",
				title: "Events",
				collapsible: true,
				html: debug.events
			}] 
		});
		this.doLayout();
	},
	updateDebugTab: function(debug) {
		var debugTab = this.getComponent(1).getComponent(2);

		debugTab.getComponent(0).body.update(debug.tasktree);
		debugTab.getComponent(1).body.update(debug.states);
		debugTab.getComponent(2).body.update(debug.events);
	},
	removeDebugTab: function() {
		this.getComponent(1).remove(2,true);
	},
	setTrace: function(trace) {
		this.debug = trace;
		this.params["_debug"] = trace ? 1 : 0;
	},
	sendTaskUpdates: function(target,updates,state) {
		//Add task updates to params
		Ext.apply(this.params, updates);
			
		//Set target and state
		this.params["_targettask"]	= target;
		this.params["_targetstate"]	= Ext.encode(state);
			
		this.refresh();
	},
	sendPropertyEvent: function(process,name,value) {
		//Ugly side-effect event handler
		this.getComponent(0).setBusy(true);
		
		Ext.Ajax.request({
			url:"/handlers/work/property",
			method: "GET",
			params: {_session : this.sessionId, process : process, property: name, value: value },
			callback: function(el,success,response,options) {
				this.getComponent(0).setBusy(false);
				this.fireEvent("propertyChanged");
				if(name == "user") //HACK: Fix with proper property events
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
			baseCls: "worktab-header",
			html: "Loading..."
		});
		itasks.WorkHeaderPanel.superclass.initComponent.apply(this,arguments);
		
	},
	setContent: function(taskid, subject, properties) {
		this.body.update( String.format(
		      "<div class=\"worktab-header-table\"><table>"
			+ "<tr><th>Subject:</th><td>{0} ({1})</td><th>Date:</th><td>{2}</td></tr>"
			+ "<tr><th>Delegated by:</th><td>{3}</td><th>Priority:</th><td>{4}</td></tr>"
			+ "</table></div><div class=\"worktab-header-indicator\"></div>"
			, subject.join(" &raquo; "), taskid, itasks.util.formatDate(properties.issuedAt)
			, properties.delegator[1], itasks.util.formatPriority(properties.priority)
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
					"change" : function(ov,nv) {var wt = this.findParentByType("itasks.work"); wt.sendPropertyEvent(wt.properties.processId,"progress",nv); }
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
	update: function (properties) {
		this.items.each(function(item){item.setValue(properties[item.name]);});
	}
});

itasks.TaskFormPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		
		Ext.apply(this,{
			taskUpdates: {},
			taskState: {},
			style: "padding: 10px",
			border: false
		});
		itasks.TaskFormPanel.superclass.initComponent.apply(this,arguments);
	},
	onRender: function () {
		itasks.TaskFormPanel.superclass.onRender.apply(this,arguments);
		this.refreshForm();
	},
	update: function (data) {
		//Update properties
		Ext.apply(this,data);
		//Update form
		this.refreshForm();
	},
	refreshForm: function() {
	
		this.body.update(this.formHtml);
		
		var forms = {};
		var prefix = "";
			
		for(var i = 0; i < this.formInputs.length; i++) {
			var ip = this.formInputs[i];
			var ec = null;
			
			//Mark the formid
			forms[ip.formid] = true;
			prefix = ip.prefix;
			
			//Construct input
			switch(ip.type) {	
				case "Int":
				case "Real":
				case "HtmlCurrency":
				case "String":
				case "HtmlPassword":
				case "HtmlTextarea":
				case "HtmlTime":
					switch(ip.type) {
						case "String":
						case "HtmlPassword": 
							ec = new Ext.form.TextField({
									inputid: ip.inputid,
									updateon: ip.updateon,
									value: ip.value,
									inputType: ip.type == "String" ? "text" : "password",
									selectOnFocus: true,
									width: 200
								});
							break;
						case "HtmlTextarea":							
							ec = new Ext.form.TextArea({
								inputid: ip.inputid,
								updateon: ip.updateon,
								value: ip.value,
								width: 500
							});
							break;
						case "HtmlTime":
							ec = new Ext.form.TimeField({
								inputid: ip.inputid,
								updateon: ip.updateon,
								value: ip.value,
								format: "H:i:s",
								width: 183
							});
							break;
						default:
							ec = new Ext.form.NumberField({
									inputid: ip.inputid,
									updateon: ip.updateon,
									value: ip.value,
									allowDecimals: (ip.type != "Int"),
									decimalPrecision: (ip.type == "HtmlCurrency" ? 2 : 100),
									selectOnFocus: true
								});
					}
					ec.on("change", function(cmp, nv, ov) {
							var wt = cmp.findParentByType("itasks.task-form");
							
							wt.addUpdate(cmp.inputid, nv);
							if(cmp.updateon == "OnChange")
								wt.sendUpdates(true);
						},this);
					break;
				case "HtmlButton":
					ec = new Ext.Button({
							inputid: ip.inputid,
							text: ip.value,
							style: "display: inline;"
						});
					ec.on("click", function(cmp, e) {
							var wt = cmp.findParentByType("itasks.task-form");
							wt.addUpdate(cmp.inputid,"click");
							wt.sendUpdates();
						},this);
					break;
				case "Bool":
				case "Maybe":
				case "HtmlCheckbox":
					ec = new Ext.form.Checkbox({
							id:	ip.prefix + ip.inputid + "-cb",
							inputid: ip.inputid,
							updateon: ip.updateon,
							checked: ip.value == "True"
						});
					ec.on("check", function (cmp, checked) {
						var wt = cmp.findParentByType("itasks.task-form");
						wt.addUpdate(cmp.inputid, checked ? "checked" : "unchecked");
						if(cmp.updateon == "OnChange")
							wt.sendUpdates(true);
					},this);
					break;
				case "HtmlDate":
					ec = new Ext.form.DateField({
						inputid: ip.inputid,
						updateon: ip.updateon,
						value: ip.value,
						width: 183
					});
					ec.on("change", function(cmp, nv, ov) {
							var wt = cmp.findParentByType("itasks.task-form");
							wt.addUpdate(cmp.inputid, nv.format("m/d/Y"));
							if(cmp.updateon == "OnChange")
								wt.sendUpdates(true);
						},this);
					break;
				case "HtmlRadiogroup":
					//Attach event handlers
					var k = 0;
					while(true) {
						var radio = Ext.get(ip.prefix + ip.inputid + '-' + k);
						
						if(radio == undefined) {
							break;
						} else {
							if(ip.updateon == "OnChange") {
								radio.on("click", function(e) {
									var wt = e.target.findParentByType("itasks.task-form");
									wt.addUpdates(e.target.name, e.target.value);
									wt.sendUpdates(true);
								},this);
							} else {
								radio.on("click", function(e) {
									e.target.findParentByType("itasks.task-form").addUpdate(e.target.name, e.target.value);
								},this);
							}
						}
						k++;
					}
					break;
				case "HtmlTimer":
					new Ext.util.DelayedTask().delay(1000 * ip.value, function () {
						var wt = this.findParentByType("itasks.task-form");
						wt.addUpdate(ip.inputid.substr(0,ip.inputid.length),"timeout");
						wt.sendUpdates();
					},this);
					
					break;
				default:
					if(ip.type.substr(0,5) == "CONS:" || ip.type == "HtmlSelect") {
						
						var getval = function(val, list) {
							for(var i = 0; i < list.length; i++) {
								if(list[i][0] == val)
									return list[i][1];
							}
							return null;
						}
						ec = new Ext.form.ComboBox({
							inputid: ip.inputid,
							updateon: ip.updateon,
							editable: false,
							triggerAction: "all",
							mode: "local",
							store: ip.options,
							value: getval(ip.value, ip.options),
							width: 183
						});
						ec.on("select", function(cmp, rec, ind) {
							var wt = cmp.findParentByType("itasks.task-form");
							wt.addUpdate(cmp.inputid, cmp.getValue());
							if(cmp.updateon == "OnChange")
								wt.sendUpdates(true);
						},this);
					}
			}
			if (ec != null) {
				ec.render(ip.prefix + ip.inputid);
			}
		}
		//Attach the submit handlers of the forms
		for(var formid in forms) {
			var form = Ext.get(prefix + formid);
			
			if(form) {
				//Cancel the form submit;
				form.dom.onsubmit = function() {return false;}
				
				//Attach replacement event handler
				form.on("submit", function (e) {
					form.findParentByType("itasks.task-form").sendUpdates();
				},this);
			}
		}	
	},
	addUpdate: function(name, value) {
		this.taskUpdates[name] = value;
	},
	sendUpdates: function(delay) {
		if(delay) {
			new Ext.util.DelayedTask().delay(150,this.sendUpdates,this);
		} else {
			var wt = this.findParentByType("itasks.work");
			if(!wt)
				return;
			
			wt.sendTaskUpdates(this.taskId,this.taskUpdates, this.taskState);
		
			this.taskUpdates = {};
		}
	}
});

itasks.TaskExtFormPanel = Ext.extend(Ext.form.FormPanel, {

	initComponent: function() {
		Ext.apply(this, {
			taskUpdates: {},
			taskState: {},
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

			var value = this.xtype == "radiogroup" ? this.getValue().value : this.getValue();
		
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
				case "numberfield":
				case "radiogroup":
					comp.on("change",changeTaskEvent);
					break;
				case "checkbox":
					comp.on("check",changeTaskEvent);
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
			new Ext.util.DelayedTask().delay(150,this.sendUpdates,this);
		} else {
			var wt = this.findParentByType("itasks.work");
			if(!wt)
				return;
			
			wt.sendTaskUpdates(this.taskId,this.taskUpdates, this.taskState);
		
			this.taskUpdates = {};
		}
	},
	update: function(data) {
		if(data.updates) {
			var i = data.updates.length - 1;
			while(i >= 0) {
				var update = data.updates[i];
				switch(update[0]) {
					case "ExtJSAdd":
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
							
							this.attachTaskHandlers(newct);
						}
						break;
					case "ExtJSRemove":
						var ct = Ext.getCmp(update[1]);
						if(ct) {
							var oct = ct.ownerCt;
							oct.remove(update[1]);
						}
						break;
				}
				i--;
			}
		}
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
					html: "Waiting for <i>" +  this.properties.subject + "</i>",
					style: "margin: 0px 0px 20px 0px;"			
				},{
					xtype: "panel",
					layout: "form",
					defaultType: "staticfield",
					items: [{
						xtype: "itasks.user",
						fieldLabel: "Assigned to",
						value: this.properties.user[1],
						listeners: {
							"change" : { fn: function(ov,nv) {this.findParentByType("itasks.work").sendPropertyEvent(this.properties.processId,"user",nv);}
									   , scope: this }
						}
					},{
						xtype: "itasks.priority",
						fieldLabel: "Priority",
						value: this.properties.priority, 
						listeners: {
							"change" : { fn: function(ov,nv) {this.findParentByType("itasks.work").sendPropertyEvent(this.properties.processId,"priority",nv);}
									   , scope: this }
						}
					},{
						fieldLabel: "Progress",
						format: itasks.util.formatProgress,
						value: this.properties.progress
					},{
						fieldLabel: "Issued at",
						format: itasks.util.formatDate,
						value: this.properties.issuedAt
					},{
						fieldLabel: "First worked on",
						format: itasks.util.formatStartDate,
						value: this.properties.firstEvent
					},{
						fieldLabel: "Last worked on",
						format: itasks.util.formatStartDate,
						value: this.properties.latestEvent
					}]
				}]
		});
		itasks.TaskWaitingPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function (data) {
		this.properties = data.properties;

		var p = data.properties;
		var props = [p.user[1],p.priority,p.progress,p.issuedAt,p.firstEvent,p.latestEvent];
		
		this.getComponent(0).body.update("Waiting for <i>" + Ext.util.Format.htmlEncode(data.properties.subject) + "</i>");
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
			"beforequery" : function(e) {e.combo.store.baseParams["_session"] = e.combo.findParentByType("itasks.work").sessionId;}
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
			layout: this.combination == "horizontal" ? "column" : null
		});
		
		//Update column sizes for horizontal layouts
		if(this.combination == "horizontal") {
			var colsize = 1.0 / this.items.length;
			for(var i = 0; i < this.items.length; i++) {
				this.items[i].columnWidth = colsize;
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
		
		//Update column sizes for horizontal layouts
		if(this.combination == "horizontal") {
			var colsize = 1.0 / items.length;
			for(var i = 0; i < items.length; i++) {
				this.items.get(i).columnWidth = colsize;
			}
		}
		this.doLayout();
	}
});

Ext.reg("itasks.work",itasks.WorkPanel);
Ext.reg("itasks.work-header",itasks.WorkHeaderPanel);
Ext.reg("itasks.work-status",itasks.WorkStatusPanel);
Ext.reg("itasks.task-form",itasks.TaskFormPanel);
Ext.reg("itasks.task-ext-form",itasks.TaskExtFormPanel);
Ext.reg("itasks.task-waiting",itasks.TaskWaitingPanel);
Ext.reg("itasks.task-combination",itasks.TaskCombinationPanel);
