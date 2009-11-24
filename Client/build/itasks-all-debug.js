/*!
 * 
 */
Ext.ns("itasks.util");

itasks.util.formatDate = function(ts) {
	if (ts == null)
		return "";
	else	
		return Date.parseDate(ts, "U").format("d M Y H:i:s");
}
itasks.util.formatStartDate = function (ts) {
	if (ts == null)
		return "Not started yet";
	else
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatDeadline = function(ts) {
	if(ts == null)
		return "No deadline";
	else
		return Date.parseDate(ts, "U").format("d M Y H:i");
}
itasks.util.formatPriority = function(priority) {
	switch(priority) {
		case null : return "";
		case "LowPriority": return "Low";
		case "NormalPriority": return "Normal";
		case "HighPriority": return itasks.util.coloredLabel("red","High");
	}
	return priority;
}
itasks.util.formatProgress = function(progress) {
	switch(progress) {
		case null: return "";
		case "TPActive" : return itasks.util.coloredLabel("green","Active");
		case "TPStuck" : return itasks.util.coloredLabel("purple","Stuck");
		case "TPWaiting": return itasks.util.coloredLabel("blue","Waiting");
		case "TPReject": return itasks.util.coloredLabel("red","Reject");
	}
	return progress;
}
itasks.util.coloredLabel = function (color, msg) {
	return "<span style=\"color: " + color + "; font-weight: bold;\">" + msg + "</span>";
}
Ext.ns("itasks.form");

itasks.form.StaticField = Ext.extend(Ext.form.Field, {

	format: Ext.util.Format.htmlEncode,
	
	initComponent: function() {
		Ext.apply(this,{
			defaultAutoCreate: {tag: "div"},
			style: "padding: 3px 0px 3px 0px"
		});
		itasks.form.StaticField.superclass.initComponent.apply(this,arguments);
	},
	onRender: function(ct, position) {
		itasks.form.StaticField.superclass.onRender.apply(this,arguments);		
		
		if(!this.el) {
			this.el = ct.createChild(this.getAutoCreate(), position);
		}
		this.setValue(this.value);
	},
	setValue: function(value) {
		this.value = value;
		if(this.rendered) {
			this.el.update(this.format(this.value));
		}
	}
});

itasks.form.InlineField = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			layout: "card",
			width: 200,
			height: 28,
			style: "margin: 0px",
			border: false,
			value: this.field.value,
			activeItem: 0,
			isFormField: true,
			items: [{
				layout: "column",
				border: false,
				items: [{
					xtype: "panel",
					style: "padding: 3px 0px 5px 0px;",
					border: false,
					columnWidth: 1,
					html: (this.format == undefined) ? this.value : this.format(this.value, this.field)
				},{
					xtype: "toolbar",
					border: false,
					width: 28,
					style: "padding: 0px 0px 0px 2px; background: none; border: 0",
					items: [{
						iconCls: "icon-edit",
						cls: "x-btn-icon",
						handler: this.startEdit,
						scope: this
					}]
				}]				
			},{
				layout: "column",
				border: false,
				items: [{
					xtype: "panel",
					border: false,
					layout: "fit",
					columnWidth: 1,
					items: [Ext.apply(this.field,{value: this.value})]
				},{
					xtype: "toolbar",
					border: false,
					width: 28,
					style: "padding: 0px 0px 0px 2px; background: none; border: 0",
					items: [{
						iconCls: "icon-accept",
						cls: "x-btn-icon",
						handler: this.stopEdit,
						scope: this
					}]
				}]		
			}]
		});
		itasks.form.InlineField.superclass.initComponent.apply(this,arguments);
		
		this.addEvents("startedit","stopedit");
	},
	startEdit: function () {
		//Switch to edit card
		this.layout.setActiveItem(1);
		this.doLayout();
		//Fire startedit event
		this.fireEvent("startedit");
	
	},
	stopEdit: function() {
		
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		var oldValue = this.value;
		var newValue = field.getValue();
		
		this.setValue(newValue);
	
		//Switch to label card
		this.layout.setActiveItem(0);
		this.doLayout();
		//Fire stopedit event
		this.fireEvent("stopedit",oldValue,newValue);
		//Fire change event
		if(oldValue != newValue)
			this.fireEvent("change",oldValue,newValue);
	},
	getValue: function() {
		return this.value;
	},
	setValue: function(value) {
		
		this.value = value;
		
		var panel = this.getComponent(0).getComponent(0);
		var field = this.getComponent(1).getComponent(0).getComponent(0);
		var label = this.format == undefined ? this.value : this.format(this.value, field)
		
		field.setValue(value);
		
		if(panel.rendered)
			panel.getEl().update(label);
		else
			panel.html = label;
	}
});

Ext.reg("staticfield", itasks.form.StaticField);
Ext.reg("inlinefield", itasks.form.InlineField);

//Global event firing. This may be used by plugins like
//Java applets, Flash or Silverlight components
fireTaskEvent = function(taskid, field, value) {
	
	var ct = Ext.getCmp("taskform-" + taskid);
	if(!ct)
		return;
	var wp = ct.findParentByType("itasks.work");
	if(!wp)
		return;
	
	wp.addEvent(field,value,ct.formState);
	wp.sendEvents();
}
/**
 * Specialized window component for iTasks login
 */

Ext.ns('itasks');

itasks.LoginWindow = Ext.extend(Ext.Window, {
	
	//Initial error message
	errorMsg: "",
	
	//Initializion function
	initComponent: function() {	
		
		var submitHandler = function() {
			this.getComponent(0).getForm().submit({waitMsg: 'Validating username and password...'});
		};
		var successHandler = function(form, action) {
			//Clear the error message
			this.getComponent(0).getComponent(0).setText('');

			//Fade out the window
			this.getEl().fadeOut({
				callback: function() {
					this.continuation(action.result.displayName, action.result.sessionId);
				},
				scope: this
			});
		};
		var failureHandler = function(form, action) {
			//Show the error and draw attention to the window
			if(action.failureType == undefined) {
				this.getComponent(0).getComponent(1).setText(action.result.error);	
			} else {
				this.getComponent(0).getComponent(1).setText("Could not connect to server");
			}
			this.getEl().frame('#ff0000');

			//Focus the username
			form.findField('username').focus(true,false);
		};
		
		Ext.apply(this, {
			y: 150,
			width: 320,
			height: 185,
			layout: "fit",
			hidden: true,
			closable: false,
			resizable: false,
			items: {
				xtype: 'form',
				url: itasks.config.serverUrl + '/authenticate',
				baseCls: 'x-plain',
				style: 'padding: 5px',
				layout: 'absolute',
				defaultType: 'textfield',
				buttonAlign: 'right',
				waitMsgTarget: true,
				items: [{
							x: 0,
							y: 0,
							xtype: 'label',
							style: "color: red; font-weight: bold; background: url('skins/" + itasks.config.skin + "/img/loginwindow.png')",
							width: 300,
							height: 40
						},{
							x: 55,
							y: 45,
							xtype: 'label',
							html: this.errorMsg,
							style: "color: red; font-weight: bold;"
						},{
							x: 0,
							y: 70,
							xtype: 'label',
							text: 'Username:'
						},{
							x: 55,
							y: 65,
							anchor: '100%',
							name: 'username'
						},{
							x: 0,
							y: 100,
							xtype: 'label',
							text: 'Password:'
						},{
							x: 55,
							y: 95,
							anchor: '100%',
							name: 'password',
							inputType: 'password'
				}],
				buttons: [{
					text: 'Log in',
					handler: submitHandler,
					scope: this
				}]
			}
		});

		//initialize the superclass (Ext.Window)
		itasks.LoginWindow.superclass.initComponent.apply(this, arguments);
		
		var loginPanel = this.getComponent(0);
	
		//Attach the success and failure event handlers
		loginPanel.on('actioncomplete', successHandler, this);
		loginPanel.on('actionfailed', failureHandler, this);

		//Add a keymap to connect <enter> in the form to the submit action
		loginPanel.on('render', function () {
			new Ext.KeyMap(loginPanel.getEl(), {
				key: Ext.EventObject.ENTER,
				fn: submitHandler,
				scope: this
			});
		},this);
	},
	focus: function() {
		this.getComponent(0).getForm().findField('username').focus();
	},
	setError: function(msg) {
		this.getComponent(0).getComponent(1).setText(msg ? msg : "");	
	},
	continuation: function(displayName, sessionId) {
	}
	
});
/**
 * Specialized progress bar for iTasks load process
 */
Ext.ns('itasks');

itasks.LoaderWindow = Ext.extend(Ext.Window, {

	progressBar: new Ext.ProgressBar({
		text: 'Initializing...'
	}),

	updateProgress: function(i, msg) {
		this.progressBar.updateProgress(i,msg);
		this.doLayout();
	},
	finish: function () {
		this.getEl().fadeOut({
			callback: function() {
				this.continuation();
			},
			scope: this
		});
	},
	initComponent: function() {
		Ext.apply(this, {
			y: 150,
			width: 350,
			height: 50,
			hidden: true,
			bodyStyle: 'padding: 5px',
			closable: false,
			resizable: false,
			items: this.progressBar
		});

		itasks.LoaderWindow.superclass.initComponent.apply(this, arguments);	
	},
	continuation: function () {
	}
});
/**
* Shared base class for panels that reflect a remote server resource.
*/
Ext.ns("itasks");

itasks.RemoteDataPanel = Ext.extend(Ext.Panel, {

	url: undefined,
	params: {},
	busy: false,
	
	initComponent: function() {		
		itasks.RemoteDataPanel.superclass.initComponent.apply(this,arguments);
		this.addEvents("remoteCallStart","remoteCallEnd");
	},
	
	/*
	* Requests a remote resource
	*/
	remoteCall: function(url,params,callback) {
		if(this.busy)
			return;
			
		this.busy = true;
		this.fireEvent("remoteCallStart");
		
		//Add session id parameter
		params["_session"] = itasks.app.session
		
		Ext.Ajax.request({
			method: 'POST',
			url: url,
			params: params,
			scripts: false,
			callback: function (el,success,response, options) {
				if(success) {
					var data;
					try {
						data = Ext.decode(response.responseText);
					} catch(SyntaxError) {
						data = response.responseText;
					}
					if(typeof data == 'object') {
						//Check for (session) errors
						if(data.error) {
							itasks.app.restart(data.error);
							return;
						}
						callback.call(this,data);
					} else {
						callback.call(this,response.responseText);
					}
					
				} else {
					var win = new Ext.Window({
						title: "Error",
						html: response.statusText,
						width: 200,
						height: 100,
						modal: true,
						closable: true,
						bodyStyle: "padding: 5px",
						buttons: [{
							text: "Ok",
							handler: function() {win.close();}
						}],
						buttonAlign: "center"
					});
					win.show();
				}
				this.busy = false;
				this.fireEvent("remoteCallEnd");
			},
			scope: this
		});
	},
	/*
	* Refreshes this panel to update the remote information
	*/
	refresh: function() {
		this.remoteCall(this.url,this.params,this.update);	
	},
	/*
	* This method must be implemented to handle a refresh event
	*/
	update: function(data) {
	}
});

Ext.reg('itasks.remotedata',itasks.RemoteDataPanel);/**
* Panel for starting new work, used to initiate new work flows
*/
Ext.ns("itasks");

itasks.NewWorkPanel = Ext.extend(Ext.tree.TreePanel ,{

	initComponent: function() {

		Ext.apply(this, {
			title: "New task...",
			iconCls: "icon-newwork",	
			loader: new Ext.tree.TreeLoader({
				dataUrl: itasks.config.serverUrl + "/new/list",
				baseParams: {_session: itasks.app.session},
				requestMethod: "POST"
			}),
			root: {text: "_ROOT_", nodeType: "async", id: "_ROOT_", expanded: true},
			rootVisible: false,
			bodyStyle: "padding-top: 2px;"
		});
		itasks.NewWorkPanel.superclass.initComponent.apply(this,arguments);
		
		this.on("click", function (node, event) {
			if(node.leaf) {
				this.startWorkflow(node.id);
			}
		},this);
	},
	startWorkflow: function (workflow) {
	
		Ext.Ajax.request({
			method: "POST",
			url: itasks.config.serverUrl + "/new/start",
			params: {_session: itasks.app.session, workflow: workflow},
			scripts: false,
			callback: this.startWorkflowCB,
			scope: this
		});
	},
	startWorkflowCB: function(el, success, response, options){
		try {
			var data = Ext.decode(response.responseText);

			this.fireEvent("processStarted",data.taskid);
		} catch(SyntaxError) {}
	}
});

Ext.reg("itasks.nwpanel", itasks.NewWorkPanel);/**
* Panel for debugging iTask workflows. Here we will put stuff such as
* task tree visualization and communication state inspection.
*/
Ext.namespace("itasks");

itasks.DebugPanel = Ext.extend(Ext.Panel, {

	worktabs: null,
	initComponent: function () {
		Ext.apply(this, {	
			title: "Debug",
			iconCls: "icon-debug",
			bodyStyle: "padding: 5px",
			deferredRender: false,
			items: [{
				xtype: "fieldset",
				title: "Overviews",
				autoHeight: true,
				items: [{
					xtype: "button",
					text: "Show task forest...",
					iconCls: "icon-task-tree",
					style: "margin-bottom: 2px;",
					listeners: {
						click: {
							fn: function() {
								this.worktabs.openTaskForestTab();
							},
							scope: this
						}
					}
				},{
					xtype: "button",
					text: "Show process table...",
					iconCls: "icon-process-table",
					listeners: {
						click: {
							fn: function() {
								this.worktabs.openProcessTableTab();
							},
							scope: this
						}
					}
				}]
			},{
				xtype: "fieldset",
				title: "Session",
				html: itasks.app.session,
				autoHeight: true,
				hideLabels: true
			}]
		});
		
		itasks.DebugPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.debug",itasks.DebugPanel);/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.grid.GridPanel, {

	initComponent: function () {
	
		var treeRenderer = function (label, meta, record) {
			var html = "";
			var level = record.data.tree_path.length;
			
			//Disable margins
			meta['attr'] = 'style="margin: 0px; padding: 0px"';
			
			//Create path
			for(var i = 0; i < level; i++) {
				if(record.data.tree_path[i]) {
					html += '<div class="treegrid treegrid-line" style="left: ' + i * 16 + 'px"></div>';
				} else {
					html += '<div class="treegrid treegrid-empty" style="left: ' + i * 16 + 'px"></div>';
				}
			}
			//Add elbow
			if(record.data.tree_last) {
				html += '<div class="treegrid treegrid-last" style="left: ' + level * 16 + 'px"></div>';
			} else {
				html += '<div class="treegrid treegrid-middle" style="left: ' + level * 16 + 'px"></div>';
			}
			//Add icon
			html += '<div class="treegrid treegrid-icon icon-' + record.data.tree_icon +'" style="left: ' + (level + 1) * 16 + 'px"></div>';
			
			//Add label
			if(record.data.tree_new) {
				html += '<div class="treegrid-label" style="font-weight: bold; left: ' + (level + 2) * 16 + 'px">' + label + '</div>';
			} else {
				html += '<div class="treegrid-label" style="left: ' + (level + 2) * 16 + 'px">' + label + '</div>';
			}
			return html;
		};
	
	
		this.workStore = new Ext.data.Store({
			autoLoad: false,
			bufferSize: 300,
			url: itasks.config.serverUrl + "/work/list",
			reader: new Ext.data.JsonReader({
					root: 'worklist',
					totalProperty: 'total',
					successProperty: 'success',
					fields: [
						{name: 'subject'},
						{name: 'priority'},
						{name: 'progress'},
						{name: 'delegatorName'},
						{name: 'timestamp'},
						{name: 'latestExtEvent'},
						{name: 'deadline'},
						{name: 'tree_path'},
						{name: 'tree_last'},
						{name: 'tree_icon'},
						{name: 'tree_new'},
						{name: 'taskid'}
					]})
			});
		
		this.workView = new Ext.grid.GridView({
			deferEmptyText: true,
			emptyText: 'There is no unfinished work.',
			nearLimit: 100,
			loadMask: { msg: 'Please wait...'}
		});
	
		Ext.apply(this, {
			border: false,
			store: this.workStore,
			view: this.workView,
			selModel: new Ext.grid.RowSelectionModel(),
			columns: [
				{id: 'subject', header: 'Subject', dataindex: 'taskid', renderer: treeRenderer, width: 200},		
				{id: 'priority', header: 'Priority', dataindex: 'priority', renderer: itasks.util.formatPriority, width: 100},
				{id: 'progress', header: 'Progress', dataindex: 'progress', renderer: itasks.util.formatProgress, width: 100},
				{id: 'delegatorName', header: 'Managed by', dataIndex: 'delegatorName', width: 100},
				{id: 'timestamp', header: 'Date', dataIndex: 'timestamp', renderer: itasks.util.formatDate, width: 120},
				{id: 'latestExtEvent', header: 'Latest Ext Event', dataIndex: 'latestExtEvent', renderer: itasks.util.formatDate, width: 120},
				{id: 'deadline', header: 'Deadline', dataIndex: 'deadline', renderer: itasks.util.formatDeadline, width: 100}
			],
			autoExpandColumn: 'subject',
			enableColumnMove: false,
			enableHdMenu: false,
			stripeRows: true,
			tbar: [{
				id: 'refreshbutton',
				xtype: 'tbbutton',
				text: 'Refresh worklist',
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
		});
		
		itasks.WorkListPanel.superclass.initComponent.apply(this, arguments);
		this.addEvents("workListRefreshed");
		
		//Check session error responses
		this.store.on('loadexception',function() {
			if(this.store.reader.jsonData.error) {
				itasks.app.restart(this.store.reader.jsonData.error);
			}
		},this);
	
		this.startAutoRefresh();
	},
	
	/*
	* Return the taskid of the selected row
	*/
	getTaskId: function (index) {
		return this.store.getAt(index).data.taskid;
	},
	/*
	* Refresh the list
	*/
	refresh: function () {
		if(this.store != null){
			this.store.load({
				params: {_session: itasks.app.session}
			});
			
			this.fireEvent("workListRefreshed", this);
		}
	},
	
	/* 
	* Start the timed task for auto-refreshing.
	*/
	startAutoRefresh: function(){
		if(itasks.config.autoRefresh){
		
			var parent = this;
		
			Ext.TaskMgr.start({
				run: function(){
					parent.refresh();
				},
				interval: itasks.config.refreshRate
			})
		}
	},
});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
/**
* Tab panel which shows a dashboard style 'home' screen.
*/

Ext.ns("itasks");

itasks.HomeTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: "Welcome",
			closable: false,
			iconCls: "icon-home",
			autoLoad: "skins/" + itasks.config.skin + "/welcome.html"
		});
		
		itasks.HomeTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg("itasks.hometab",itasks.HomeTabPanel);/**
* Tab panel which shows the global task forest.
*/

Ext.ns("itasks");

itasks.TaskForestTabPanel = Ext.extend( Ext.Panel, {
	
	initComponent: function () {
		
		this.tree = new Ext.ux.tree.ColumnTree({
			id : 'treepanel',
			region : 'center',
			
			rootVisible: false,
			autoScroll: true,
			title: "Task Overview",

			border: false,
			bodyStyle: "background-color: white",
			
			iconCls : "icon-task-tree",
			
			columns:[{
						header:'Task Id',
						dataIndex:'taskId',
						width: 500
					},{
						header:'Assigned to..',
						dataIndex:'user',
						width: 100
					},{
						header:'Task Label',
						dataIndex:'taskLabel',
						width: 200
					},{
						header:'Trace Value',
						dataIndex: 'traceValue',
						width: 200
					},{
						header: ''
					}],	
			
			loader: new Ext.tree.TreeLoader({
						dataUrl: itasks.config.serverUrl + "/debug/taskforest",
						baseParams: {_session: itasks.app.session},
						requestMethod: "POST",
						uiProviders: { col : Ext.ux.tree.ColumnNodeUI }
					}),	
			
			root: new Ext.tree.AsyncTreeNode({
					text:'Tasks'
				})			
		});
		
		this.iconPanel = new Ext.Panel({
			id: 'iconpanel', 
			region: 'south',
			height: 20,
			border: false,
			
			html: '<div class="taskForestLegend task-mnt">Main Task</div>'+
			      '<div class="taskForestLegend task-int">Edit Task</div>'+
				  '<div class="taskForestLegend task-mon">Monitor Task</div>'+
				  '<div class="taskForestLegend task-fin">Finished Task</div>'+
				  '<div class="taskForestLegend task-seq">Sequential Combinator</div>'+
				  '<div class="taskForestLegend task-par">Parallel Combinator</div>'+
				  '<div class="taskForestLegend task-rpc">Remote Procedure Call</div>'+
				  '<div class="x-clear"></div>'
		
		});
						
		Ext.apply(this, {
			
			title: "Task forest",
			closable: true,
			autoScroll: false,
			
			cls: "worktab-container",
			iconCls: "icon-task-tree",
			
			layout: "border",
			bodyStyle: "background-color: white",
			
			items: [this.tree, this.iconPanel],
			
			tbar: [{
					text: "Refresh",
					iconCls: "x-tbar-loading",
					listeners: {click: {fn: function (btn) {this.refresh();},scope: this}}
				  },{
				    text: "(Un)fold tree",
					iconCls: "icon-collapse-tree",
					listeners: {click: {fn: function (btn) {this.toggleTree();},scope: this}}
				  }]			
		});
		
		itasks.TaskForestTabPanel.superclass.initComponent.apply(this, arguments);
	},
	
	refresh: function(){
		this.tree.getRootNode().reload();
	},
	
	toggleTree: function(){
		var rootNode = this.tree.getRootNode();
		
		toggleChild = function(child){
			if(child.isExpanded()){
				child.collapse(true,false);
			}else{
				child.expand(true,true);
			}
		}
		
		rootNode.eachChild(toggleChild);
		
	}
});

Ext.reg("itasks.taskforesttab",itasks.TaskForestTabPanel);/**
* Tab panel which shows the global process table.
*/

Ext.ns('itasks');

itasks.ProcessTableTabPanel = Ext.extend(itasks.RemoteDataPanel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: "Process table",
			closable: true,
			autoScroll: true,
			url: itasks.config.serverUrl + "/debug/processtable",
			cls: "worktab-container",
			iconCls: "icon-process-table",
			bodyStyle: "padding: 10px;",
			tbar: [{
					text: "Refresh",
					iconCls: "x-tbar-loading",
					listeners: { click: { fn: function (btn) {this.refresh();}, scope: this}}
				}]
		});	
		itasks.ProcessTableTabPanel.superclass.initComponent.apply(this, arguments);
	},
	update: function(data) {
		this.body.dom.innerHTML = data;
	}
});

Ext.reg("itasks.processtabletab",itasks.ProcessTableTabPanel);/**
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
			
			url: itasks.config.serverUrl + "/work/tab",
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
			border: false
		});
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
		
		this.doLayout();
	}
});

Ext.reg("itasks.work",itasks.WorkPanel);
Ext.reg("itasks.work-header",itasks.WorkHeaderPanel);
Ext.reg("itasks.work-status",itasks.WorkStatusPanel);
Ext.reg("itasks.task-ext-form",itasks.TaskExtFormPanel);
Ext.reg("itasks.task-monitor",itasks.TaskMonitorPanel);
Ext.reg("itasks.task-waiting",itasks.TaskWaitingPanel);
Ext.reg("itasks.task-combination",itasks.TaskCombinationPanel);/**
* This panel will show the current work in tabs.
*/

Ext.ns("itasks");

itasks.WorkTabsPanel = Ext.extend(Ext.TabPanel, {
	
	initComponent: function() {
		Ext.apply(this,{
			activeItem: 0,
			layoutOnTabChange: true,
			items: {xtype: "itasks.hometab"}
		});
		
		itasks.WorkTabsPanel.superclass.initComponent.apply(this,arguments);
	},
	openWorkTab: function (taskid) {

		//Id is prefixed with the string "worktab-"
		var id = "worktab-" + taskid;
		
		//Try to find an existing tab with the same id
		var tab = this.getComponent(id);
		var isnew = false;
		
		if(tab == undefined) {
			//Create new tab
			isnew = true;
			tab = new itasks.WorkPanel({id: id, taskId: taskid});
			
			//Add new tab
			this.add(tab);
			this.activate(tab);
			this.doLayout();
		}
		this.activate(tab);
				
		//Return a reference to the new tab
		return [tab,isnew];
	},
	openDebugTab: function() {
		var tab = this.getComponent("debugtab");
		if(tab == undefined) {
			tab = new itasks.DebugPanel({id: "debugtab", worktabs: this, closable: true});
			this.add(tab);
		}
		this.activate(tab);
		return tab;	
	},
	openTaskForestTab: function () {
		var tab = this.getComponent("taskforesttab");
		if(tab == undefined) {
			tab = new itasks.TaskForestTabPanel({id: "taskforesttab"});
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	},
	openProcessTableTab: function () {
		var tab = this.getComponent("processtabletab");
		if(tab == undefined) {
			tab = new itasks.ProcessTableTabPanel({id: "processtabletab"});
			this.add(tab);
		}
		this.activate(tab);
		tab.refresh();
		return tab;
	}
});

Ext.reg("itasks.worktabs",itasks.WorkTabsPanel);/**
* Main iTasks user interface
*/
Ext.ns('itasks');

itasks.ApplicationPanel = Ext.extend(Ext.Panel, {

	initComponent: function() {

		Ext.apply(this, {
			layout: 'border',
			hidden: true,
			hideMode: 'offsets',
			deferredRender: false,
			items: [{
					id: 'northpanel',
					xtype: 'panel',
					region: 'north',
					height: 40,
					html: '<div id="logo" ></div><div id="user">Welcome ' + itasks.app.displayName + ' | <a id="logout" href="javascript:void(0);">Log out &raquo;</a></div>'
				},{
					id: 'leftpanel',
					xtype: 'itasks.nwpanel',
					region: 'west',
					layoutConfig: {animate: true},
					collapsible: true,
					split: true,
					border: false,
					deferredRender: false,
					width: 200,
					minWidth: 200,
					maxWidth: 400
				},{
					id: 'centerpanel',
					region: 'center',
					xtype: 'panel',
					layout: 'border',
					border: false,
					deferredRender: false,
					items: [ {
						id: 'worklist',
						xtype: 'itasks.worklist',
						region: 'north',
						split: true,
						height: 150
					},{
						id: 'worktabs',
						xtype: 'itasks.worktabs',
						border: false,
						region: 'center'
					}]
				}]
		});
	
		itasks.ApplicationPanel.superclass.initComponent.apply(this, arguments);
	},
	init: function () {	
		//Initializing the gui...
		var apppanel	= this;
		
		var newpanel 	= this.getComponent('leftpanel');
		var worklist 	= this.getComponent('centerpanel').getComponent('worklist');
		var worktabs 	= this.getComponent('centerpanel').getComponent('worktabs');
	
		//Refresh initial overviews
		worklist.refresh();
		
		//Connect event handlers
		Ext.get("logout").on("click",function() {
			apppanel.logout();
		});
		
		var attachTabHandlers = function(tab) {
			if(tab[1]) { //The tab is new
				tab[0].on("taskDone",function(taskid) {
					worklist.refresh();
				});
				tab[0].on("taskRedundant",function(taskid) {
					worklist.refresh();
				});
				tab[0].on("propertyChanged",function(taskid) {
					worklist.refresh();
				});
			}
			tab[0].refresh();
		};
		worklist.on("cellclick",function (grid,row,col,event) {
			attachTabHandlers(worktabs.openWorkTab(grid.getTaskId(row)));
		});
		
		worklist.on("workListRefreshed",function(worklist) {
			worklist.workStore.each(function(){
				var tab = worktabs.getComponent("worktab-"+this.data.taskid);
				var wlTStamp = this.data.latestExtEvent;
				
				if(tab != null){
					var tTStamp = tab.properties.systemProps.latestEvent
				
					if(wlTStamp > tTStamp){
						tab.refresh();
					}				
				}				
			});
		});
		
		newpanel.on("processStarted",function(taskid) {
			//When new work is started, refresh the worklist
			//and immediately open a tab for the work
			worklist.refresh();	
			attachTabHandlers(worktabs.openWorkTab(taskid));
		},this);
		
		//Fix for tabpanel resize event bug
		worktabs.on("resize",function(c) {
			c.doLayout();
		});
		
		//Add debug button
		if(itasks.config.debug) {
		
			var tb = worklist.getTopToolbar()
			var button = {
				xtype: "tbbutton",
				text: "Debug...",
				iconCls: "icon-debug",
				listeners: {
					click: {
						fn: function() {
							worktabs.openDebugTab();
						},
						scope: this
					}
				}
			};
			if(worklist.rendered) {
				tb.add(button);
			} else {
				tb[tb.length] = button;
			}
		}
	},
	logout: function() {	
		//Send logout request to the server
		Ext.Ajax.request({
			url: itasks.config.serverUrl + "/deauthenticate",
			method: "POST",
			params: {session: itasks.app.session},
			scripts: false,
			callback: function () {
				//On return, restart the app
				itasks.app.restart();
			},
			scope: this
		});
	}
});/**
* Main application
*/
Ext.ns('itasks');

itasks.Application = function () {
	return {
		//Application-wide state
		session: null,
		displayName: null,
		
		viewport: new Ext.Viewport({
			layout: 'card',
			activeItem: 0,
			layoutConfig: {
				deferredRender: false
			},
			items: {
				baseCls: 'bg',
				xtype: 'panel'
			}	
		}),
		
		loginWindow: null,
		loaderWindow: null,
		mainGui: null,
		/**
		* Starts the client GUI framework
		*/
		start: function(errorMsg) {
			//Store message
			this.errorMsg = errorMsg;
			
			//Load the config
			this.loadConfig();
		},
		loadConfig: function() {
			Ext.Ajax.request({url:"config.json",success: this.continueConfig, scope: this});
		},
		continueConfig: function(response) {
			//Globally store config
			itasks.config = Ext.decode(response.responseText);
			
			//Load skin
			this.loadSkin();
			
			//Create the login window
			if(!this.loginWindow) {
				this.loginWindow = new itasks.LoginWindow({
					errorMsg: this.errorMsg,
					continuation:  this.loadUserInterface.createDelegate(this)
					});
				this.viewport.getComponent(0).add(this.loginWindow);
			} else {
				this.loginWindow.setError(this.errorMsg);
			}
			
			this.loginWindow.show();
		},
		loadSkin: function() {
			var link = document.createElement("link");
			link.rel = "stylesheet";
			link.type = "text/css";
			link.href = "skins/" + itasks.config.skin + "/main.css";
			
			document.body.appendChild(link);
			document.title = itasks.config.appTitle;
		},	
		/**
		* Loads and builds the GUI
		*/	
		loadUserInterface: function(displayName, session) {
			
			//Update global state
			this.session = session;
			this.displayName = displayName;
			
			//Remove the login window
			this.loginWindow.hide();
		
			var startPanel = this.viewport.getComponent(0);
			
			//Create the loader window
			if(!this.loaderWindow) {
				this.loaderWindow = new itasks.LoaderWindow({
					continuation: this.startUserInterface.createDelegate(this)
				});
				this.viewport.getComponent(0).add(this.loaderWindow);
			
			} else {
				this.loaderWindow.updateProgress(0.0,'Initializing');
			}
				
			this.loaderWindow.show();
			
			//Start building the GUI
			this.loaderWindow.updateProgress(0.2,'Building User Interface...');
	
			this.mainGui = new itasks.ApplicationPanel();
			this.viewport.add(this.mainGui);
			this.viewport.doLayout();
				
			this.loaderWindow.updateProgress(0.6,'Initializing User Interface...');
			this.mainGui.init();
			
			//Finish the loader
			this.loaderWindow.updateProgress(1.0,'Done.');
			this.loaderWindow.finish();
		},
		/**
		* Starts the main interface
		*/
		startUserInterface: function() {
			//Remove the loader window and startpanel
			this.loaderWindow.hide();
			
			//Switch to the main interface
			this.viewport.layout.setActiveItem(1);
		},
		/**
		* Resets the main viewport to show the start screen
		*/
		reset: function() {
			this.viewport.layout.setActiveItem(0);
			this.viewport.remove(1,true);
		},
		restart: function (errorMsg) {
			this.reset();
			this.start(errorMsg);
		}
	}
};Ext.ns("itasks");

itasks.GMapPanel = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.applyIf(this,
			{ url: "/handlers/work/tab"
			, zoom: 15
			, center : [51.824118,5.868174]
			, mapType : "ROADMAP"
            , border: false
			, autoHeight: false
			, height: 400
			, width: 500
			, scope: this
			, displayedMarkers : new Array()
			});

		itasks.GMapPanel.superclass.initComponent.apply(this,arguments);
		
		this.show();
	},
	
	setValue : function(_data){
		var data = Ext.decode(_data);
		//this.gmap.setCenter(new google.maps.LatLng(data.center[0],data.center[1]));
		//this.gmap.setZoom(data.zoom);
		//this.gmap.setMapTypeId(this.getMapType(data.mapType));
		this.markers = data.markers;
		this.addMarkers();
	},
	
	getMapType : function (mapType){
		return eval("google.maps.MapTypeId."+mapType);
	},

	afterRender : function(){
				
		itasks.GMapPanel.superclass.afterRender.call(this);  
		
		
		var options = 
			{ center : new google.maps.LatLng(this.center[0],this.center[1])
			, zoom: this.zoom
			, mapTypeId : this.getMapType(this.mapType)
		}
		
		this.gmap = new google.maps.Map(this.body.dom, options);

		this.addMarkers()
		
		var parent = this;
		
		var mvcEventHandler = function(){
			
			var ll = parent.gmap.getCenter();
			var zm = parent.gmap.getZoom();
			
			var value = {
				center : [ll.lat(),ll.lng()],
				zoom   : zm,
				type   : parent.gmap.getMapTypeId().toUpperCase()
			}
			
			var ct = parent.findParentByType("itasks.task-ext-form");
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();
		}
		
		var lclickEventHandler = function(event){
			
			var ll = event.latLng
			
			var value = {
				event 	: "LEFTCLICK",
				source 	: "MAP",
				point	: [ll.lat(),ll.lng()]
			}
			
			var ct = parent.findParentByType("itasks.task-ext-form");
			if(!ct) return;
			
			ct.addUpdate(parent.name, Ext.encode(value));
			ct.sendUpdates();			
		}
				
		if(this.isEditor){
			google.maps.event.addListener(this.gmap, 'maptypeid_changed', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'idle', mvcEventHandler);
			google.maps.event.addListener(this.gmap, 'click', lclickEventHandler);
		}
				
	},
	
	addMarkers : function (){
		var i=0;
		for(i=0; i<this.displayedMarkers.length; i++){
			this.displayedMarkers[i].setMap(null);
		}
		
		this.displayedMarkers = new Array();
		
		for(i=0; i<this.markers.length; i++){
						
			var markerObj = new google.maps.Marker({
				map : this.gmap,
				position : new google.maps.LatLng(this.markers[i].position[0],this.markers[i].position[1])
			});	

			this.displayedMarkers[i] = markerObj;
		}		
	}
});

Ext.reg('itasks.gmappanel', itasks.GMapPanel);Ext.ns("itasks");

itasks.GStaticMapPanel = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.apply(this,
			{ border: false
			, autoHeight: false
			, html: '<img src="'+this.url+'" width="'+this.width+'" height="'+this.height+'">'
			});

		itasks.GMapPanel.superclass.initComponent.apply(this,arguments);
		
		this.show();
	}
});

Ext.reg('itasks.gstaticmappanel', itasks.GStaticMapPanel);Ext.BLANK_IMAGE_URL = "/ext3/resources/images/default/s.gif";

Ext.ns("itasks");

Ext.onReady(function(){
	itasks.app = new itasks.Application();
	itasks.app.start();
});
