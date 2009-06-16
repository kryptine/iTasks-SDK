/**
* Tab panel which shows a task a user is working on
*/

Ext.ns('itasks');

itasks.InlineEditField = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			layout: 'card',
			border: false,
			value: this.field.value,
			activeItem: 0,
			
			items: [{
				layout: 'column',
				border: false,
				items: [{
					xtype: 'panel',
					style: 'padding: 3px 0px 0px 2px;',
					border: false,
					columnWidth: 1,
					html: (this.format == undefined) ? this.field.value : this.format(this.field.value, this.field)
				},{
					xtype: 'toolbar',
					border: false,
					cls: 'x-form-item',
					width: 28,
					height: 24,
					style: 'padding: 0px 0px 0px 2px; background: none; border: 0',
					items: [{
						icon: 'img/icons/pencil.png',
						cls: 'x-btn-icon',
						handler: this.startEdit,
						scope: this
					}]
				}]				
			},{
				layout: 'column',
				border: false,
				items: [{
					xtype: 'panel',
					border: false,
					layout: 'fit',
					columnWidth: 1,
					items: [this.field]
				},{
					xtype: 'toolbar',
					border: false,
					width: 28,
					style: 'padding: 0px 0px 0px 2px; background: none; border: 0',
					items: [{
						icon: 'img/icons/accept.png',
						cls: 'x-btn-icon',
						handler: this.stopEdit,
						scope: this
					}]
				}]		
			}]
		});
		itasks.InlineEditField.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('startedit','stopedit');
	},
	startEdit: function () {
		//Switch to edit card
		this.layout.setActiveItem(1);
		//Fire startedit event
		this.fireEvent('startedit');
	},
	stopEdit: function() {
		
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		var oldValue = this.value;
		var newValue = field.getValue();
		
		this.value = newValue;
		
		//Update the label
		this.setLabel((this.format == undefined) ? this.value : this.format(this.value, field));
	
		//Switch to label card
		this.layout.setActiveItem(0);
		
		//Fire stopedit event
		this.fireEvent('stopedit',oldValue,newValue);
	},
	setLabel: function(msg) {
		this.getComponent(0).getComponent(0).getEl().update(msg);
	},
	getValue: function() {
		return this.value;
	}
});

Ext.reg('inlinefield', itasks.InlineEditField);


itasks.WorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: null,
	sessionId: null,
	application: null,
	
	debug: false,
	initialized: false,
	
	idataEvents: [],
	idataStates: [],
	
	initComponent: function() {
		Ext.apply(this, {
			title: "Loading task...",
			closable: true,
			iconCls: 'icon-editTask',
			
			url: '/handlers/work/tab',
			params: {task: this.taskId, debug: this.debug ? 1 : 0, prefix: 'ip-0-'},
			nextPrefix: 1,
			
			layout: 'anchor',
			deferredRender: false,
			items: [{
				xtype: 'itasks.work-header',
				height: 50,
				anchor: 'r'
			},{
				xtype: 'tabpanel',
				anchor: 'r -50',
				cls: 'worktab-container',
				tabPosition: 'bottom',
				layoutOnTabChange: true,
				activeTab: 0,
				items: [{
					title: 'Task',
					iconCls: 'icon-editTask',
					layout: 'fit',
					hideMode: 'offsets',
					border: false,
					autoWidth: true,
					autoScroll: true
				}],
				tbar: [{
						text: 'Refresh task',
						iconCls: 'x-tbar-loading',
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
			
			if(ct.items.length > 0)
				ct.remove(0,true);
				
			switch(data.content) {
				case "done":	
					ct.add(new itasks.WorkMessagePanel({
						html: "This task is completed. Thank you."
					}));
					break;
				case "redundant":
					ct.add(new itasks.WorkMessagePanel({
						html: "The completion of this task is no longer required.<br />It has been removed. Thank you for your effort."
					}));
					break;	
			}
			ct.doLayout();			
			return;
		}
		//Update header
		this.getComponent(0).setContent(this.taskId, data.subject, data.properties);
		//Update title
		this.updateTitle(data.subject);
		//Update debug tab
		this.updateDebug(data.debug);
		//Update content
		this.updateContent(data.content);
	
		//Reset params, events and states
		this.params = { task : this.taskId
					  , debug: this.debug ? 1 : 0
					  , prefix: 'ip-' + this.nextPrefix++ + '-'
					  }
		this.idataEvents = [];
		this.idataStates = [];
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
	updateDebug: function(debug) {
		if(debug != null) {
			if(this.getComponent(1).items.length > 1) {
				if(this.getComponent(1).getComponent(1).rendered) {
					this.updateDebugTab(debug);
				} else {
					this.removeDebugTab(debug);
					this.addDebugTab(debug);
				}
			} else {
				this.addDebugTab(debug);
			}
		} else {
			if(this.getComponent(1).items.length > 1)
				this.removeDebugTab();
		}
	},
	addDebugTab: function(debug) {
		this.getComponent(1).add({
			title: 'Debug',
			iconCls: 'icon-debug',
			bodyStyle: 'padding: 10px',
			autoScroll: true,
			autoWidth: true,
			items: [{
				xtype: 'panel',
				title: 'Task tree',
				collapsible: true,
				html: debug.tasktree
			},{
				xtype: 'panel',
				title: 'Task states',
				collapsible: true,
				html: debug.states
			},{
				xtype: 'panel',
				title: 'Events',
				collapsible: true,
				html: debug.events
			}] 
		});
		this.doLayout();
	},
	updateDebugTab: function(debug) {
		var debugTab = this.getComponent(1).getComponent(1);

		debugTab.getComponent(0).body.update(debug.tasktree);
		debugTab.getComponent(1).body.update(debug.states);
		debugTab.getComponent(2).body.update(debug.events);
	},
	removeDebugTab: function() {
		this.getComponent(1).remove(1,true);
	},
	setTrace: function(trace) {
		this.debug = trace;
		this.params['debug'] = trace ? 1 : 0;
	},
	addEvent: function(input, value, states) {
		//Add an event to the list of events
		this.idataEvents[input] = value;
		
		//Add form states
		this.idataStates = this.idataStates.concat(states);
	},
	sendEvents: function(delayed) {
		
		if(delayed) {
			new Ext.util.DelayedTask().delay(150,this.sendEvents,this);
		} else {	
			//Add idata events to params
			Ext.apply(this.params, this.idataEvents);
			
			//Add idata states events to params
			this.params['state'] = Ext.encode(this.idataStates);
			
			this.refresh();
		}
	}
});

itasks.WorkHeaderPanel = Ext.extend(Ext.Panel, {
		
	initComponent: function() {
		Ext.apply(this, {
			deferredRender: false,
			items: [{
				xtype: 'panel',
				anchor: '0 0',
				baseCls: 'worktab-header',
				html: "Loading..."
			},{
				xtype: 'panel',
				anchor: '-50 100%',
				baseCls: 'worktab-header',
				html: "<div class=\"worktab-header-indicator\"></div>"
			}]
		});
		itasks.WorkHeaderPanel.superclass.initComponent.apply(this,arguments);
		
	},
	setContent: function(taskid, subject, properties) {
		this.getComponent(0).body.update( String.format(
		      "<div class=\"worktab-header-table\"><table>"
			+ "<tr><th>Subject:</th><td>{0} ({1})</td><th>Date:</th><td>{2}</td></tr>"
			+ "<tr><th>Delegated by:</th><td>{3}</td><th>Priority:</th><td>{4}</td></tr>"
			+ "</table></div>"
			, subject.join(" &raquo; "), taskid, itasks.util.formatDate(properties.issuedAt)
			, properties.delegator[1], itasks.util.formatPriority(properties.priority)
			));
	},
	setBusy: function(busy) {
		this.getComponent(1).getEl().child(".worktab-header-indicator").setVisible(busy);
	}	
});

itasks.WorkMessagePanel = Ext.extend(Ext.Panel, {
	
	timeout: 5,
	
	initComponent: function() {
		Ext.apply(this, {
			bodyStyle: 'padding: 10px',
			items: [{
				xtype: 'panel',
				border: false,
				html: this.html
			},{
				xtype: 'progress',
				style: 'margin: 10px 0px 0px 0px;',
				value: 1.0,
				text: 'This window will automatically close in ' + this.timeout + ' seconds'
			}],
			html: null
		});
		itasks.WorkMessagePanel.superclass.initComponent.apply(this,arguments);
		
		new Ext.util.DelayedTask().delay(1000,this.update,this,[this.timeout - 1]);
	},
	update: function(timeout) {
		if(timeout == 0) {
			var pt = this.findParentByType('itasks.work');
			if(pt.ownerCt)
				pt.ownerCt.remove(pt);
		} else {
			var pb = this.getComponent(1);
			
			pb.updateText('This window will automatically close in ' + timeout + ' seconds');
			pb.updateProgress( (1.0 * timeout) / this.timeout);
			
			new Ext.util.DelayedTask().delay(1000,this.update,this,[timeout - 1]);
		}
	}
});

itasks.TaskFormPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		
		Ext.apply(this,{
			style: 'padding: 10px',
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
							var wt = this.findParentByType('itasks.work');
							wt.addEvent(cmp.inputid, nv, this.formState);
							if(cmp.updateon == "OnChange")
								wt.sendEvents(true);
						},this);
					break;
				case "HtmlButton":
					ec = new Ext.Button({
							inputid: ip.inputid,
							text: ip.value,
							style: "display: inline;"
						});
					ec.on("click", function(cmp, e) {
							var wt = this.findParentByType('itasks.work');
							wt.addEvent(cmp.inputid,"click",this.formState);
							wt.sendEvents();
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
						var wt = this.findParentByType('itasks.work');
						wt.addEvent(cmp.inputid, checked ? "checked" : "unchecked", this.formState);
						if(cmp.updateon == "OnChange")
							wt.sendEvents(true);
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
							var wt = this.findParentByType('itasks.work');
							wt.addEvent(cmp.inputid, nv.format("m/d/Y"), this.formState);
							if(cmp.updateon == "OnChange")
								wt.sendEvents(true);
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
									var wt = this.findParentByType('itasks.work');
									wt.addEvent(e.target.name, e.target.value, this.formState);
									wt.sendEvents(true);
								},this);
							} else {
								radio.on("click", function(e) {
									this.findParentByType('itasks.work').addEvent(e.target.name, e.target.value, this.formState);
								},this);
							}
						}
						k++;
					}
					break;
				case "HtmlTimer":
					new Ext.util.DelayedTask().delay(1000 * ip.value, function () {
						var wt = this.findParentByType('itasks.work');
						wt.addEvent(ip.inputid.substr(0,ip.inputid.length),"timeout", this.formState);
						wt.sendEvents();
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
							var wt = this.findParentByType('itasks.work');
							wt.addEvent(cmp.inputid, cmp.getValue(), this.formState);
							if(cmp.updateon == "OnChange")
								wt.sendEvents(true);
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
					this.findParentByType('itasks.work').sendEvents();
				},this);
			}
		}		
	}
});

//Waiting for main task panel
itasks.TaskWaitingPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
				
		Ext.apply(this, {
			cls: 'worktab-content',
			border: false,
			items: [{
					xtype: 'panel',
					style: 'padding: 5px;',
					border: false,
					cls: 'x-form-item x-form-item-label',
					html: 'Waiting for <i>' +  this.properties.subject + '</i>'				
				},{
					layout: 'column',
					border: false,
					items: [{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px; background: none;',
						width: 100,
						text: 'Assigned to:'
					},{
						xtype: 'inlinefield',
						width: 300,
						field: {
							xtype: 'combo',
							value: this.properties.user[1],
							label: this.properties.user[1],
							store: new Ext.data.JsonStore({
								root: 'users',
								totalProperty: 'total',
								fields: ['userId','displayName'],
								url: '/handlers/data/users'
							}),
							displayField: 'displayName',
							valueField: 'userId',
							triggerAction: 'all',
							editable: false,
							forceSelection: true,
							listeners: {
								'select' : {fn: function(cmt,rec,ind) {cmt.label = rec.get('displayName'); }, scope: this}
							}
						},
						format: function(value, field) {
							return field.label;
						},
						listeners: {
							'stopedit' : {fn: this.setUser, scope: this}
						}
					}]
				},{
					layout: 'column',
					border: false,
					items: [{
						width: 100,
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						text: 'Priority:'
					},{
						xtype: 'inlinefield',
						width: 300,
						format: itasks.util.formatPriority,
						field: {
							xtype: 'combo',
							value: this.properties.priority,
							store: [['HighPriority','High'],['NormalPriority','Normal'],['LowPriority','Low']],
							editable: false,
							triggerAction: 'all',
							forceSelection: true
						},
						listeners: {
							'stopedit' : {fn: this.setPriority, scope: this}
						}
					}]
				},{
					layout: 'column',
					border: false,
					items: [{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						width: 100,
						text: 'Issued at:'
					},{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						text: itasks.util.formatDate(this.properties.issuedAt),
						columnWidth: 1	
					}]
				},{
					layout: 'column',
					border: false,
					items: [{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						width: 100,
						text: 'First worked on:'
					},{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						text: this.properties.firstEvent == null ? 'Not started yet' : itasks.util.formatDate(this.properties.firstEvent),
						columnWidth: 1	
					}]
				},{
					layout: 'column',
					border: false,
					items: [{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						width: 100,
						text: 'Last worked on:'
					},{
						xtype: 'label',
						cls: 'x-form-item x-form-item-label',
						style: 'padding: 5px 0px 5px 5px;',
						text: this.properties.latestEvent == null ? 'Not started yet' : itasks.util.formatDate(this.properties.latestEvent),
						columnWidth: 1	
					}]
				}]
		});
		
		itasks.TaskWaitingPanel.superclass.initComponent.apply(this,arguments);
		
		this.on('render',function() {	
			var sessionId = this.findParentByType('itasks.work').sessionId;
			this.sessionId = sessionId;
			this.getComponent(1).getComponent(1).field.store.baseParams = {session: sessionId}
		},this);
	},
	setUser: function(ov,nv) {
		if(ov != nv) { //Ugly side-effect event handler
			Ext.Ajax.request({
				url:"/handlers/work/property",
				method: "GET",
				params: {session : this.sessionId, process : this.properties.processId, property: "user", user: nv }
			});
		}
	},
	setPriority: function(ov,nv) {
		if(ov != nv) { //Ugly side-effect event handler
			Ext.Ajax.request({
				url:"/handlers/work/property",
				method: "GET",
				params: {session : this.sessionId, process : this.properties.processId, property: "priority", priority: nv }
			});
		}
	},
	
	update: function (data) {
		//TODO: Update properties on task refresh
	}
});

//Combination of other tasks
itasks.TaskCombinationPanel = Ext.extend(Ext.Panel, {
	initComponent: function() {
		Ext.apply(this, {
			border: false,
			layout: this.combination == "horizontal" ? "table" : null,
			layoutConfig: this.combination == "horizontal" ? {columns : this.items.length } : null
		});
		itasks.TaskCombinationPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function (data) {
		//Check and update all items
		var i = 0;
		var items = data.items;
	
		while(i < this.items.length) {
			if(i >= items.length) {
				this.remove(i);
				continue;
			}
			if(items[i].taskId == this.items.get(i).taskId) {
				this.items.get(i).update(items[i]);
			} else {
				this.insert(i,items[i]);
			}
			i++;
		}
		this.doLayout();
	}
});

Ext.reg('itasks.work',itasks.WorkPanel);
Ext.reg('itasks.work-header',itasks.WorkHeaderPanel);
Ext.reg('itasks.task-form',itasks.TaskFormPanel);
Ext.reg('itasks.task-waiting',itasks.TaskWaitingPanel);
Ext.reg('itasks.task-combination',itasks.TaskCombinationPanel);
