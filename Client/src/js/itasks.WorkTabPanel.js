/**
* Tab panel which shows a task a user is working on
*/

Ext.ns("itasks");

itasks.WorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	taskId: null,
	properties: null,
	
	debug: false,
	
	initComponent: function() {
		Ext.apply(this, {
			title: "Loading task...",
			closable: true,
			autoDestroy: true,
			iconCls: "icon-task",
			url: itasks.config.serviceUrl + "/json/tasks/" + this.taskId + "/tui",
			params: {session: itasks.app.session},
			listeners:	{ tuichange: {fn:this.onTuiChange, scope: this}
						, tuiaction: {fn:this.onTuiAction, scope: this}
						},
			layout: "border",
			items: [{
				xtype: "itasks.work-header",
				region: "north",
				height: 25
			},{
				xtype: "panel",
				region: "center",
				ctCls: "worktab-container",
				bodyStyle: 'background-color: #eee',
				tbar: [{
					text: 'Task Actions',
					iconCls: 'icon-properties',
					menu: {
						id: 'taskMenu',
						items: [{
							text: 'Refresh Task',
							iconCls: 'x-tbar-loading',
							scope: this,
							handler: function(item,evt){
								this.refresh(false);
							}
						},{
							text: 'Task Properties'	,
							iconCls: 'icon-properties',
							scope: this,
							handler: function(item,evt){
								this.showProperties();
							}
						},{
							text: 'Discuss Task',
							iconCls: 'icon-chat',
							disabled: true
						},'-',{
							text: 'Cancel Task',
							iconCls: 'icon-trash',
							scope: this,
							handler: function(item,evt){
								this.cancel();
							}
						}]
					}					
				}]
			}]
		});
			
	
		itasks.WorkPanel.superclass.initComponent.apply(this, arguments);
	
		this.addEvents("taskRedundant","taskDone","propertyChanged","afterUpdateContent");
		
		this.on("afterUpdateContent",function(){
			this.doLayout(false,true);
		});
		
		//Attach event handlers for the loading indicator
		this.on("remoteCallStart",function() {
			this.getComponent(0).setBusy(true);
		},this);
		this.on("remoteCallEnd",function() {
			this.getComponent(0).setBusy(false);
		},this);
	},
	onTuiChange: function(taskId,name,value) {
		if (!Ext.isDefined(this.params['editEvent'])) {
			this.params['editEvent'] = Ext.encode([taskId,name,value]);
			if (!Ext.isDefined(this.params['commitEvent'])) {
				// introduce slight delay to possibly send commit event in same request
				this.sendEvents.defer(100,this);
			}
		}	
	},
	onTuiAction: function(taskId,value) {
		if (!Ext.isDefined(this.params['commitEvent'])) {
			this.params['commitEvent'] = Ext.encode([taskId,value]);
			if (!Ext.isDefined(this.params['editEvent'])) {
				this.sendEvents();
			}
		}
	},
	sendEvents: function() {
		this.refresh(true);
		delete(this.params['editEvent']);
		delete(this.params['commitEvent']);
	},
	update: function(data,success,status) {
		if(!success) {
			if (status == 404) {
				// if process not found, fade out tab
				this.fadeOut();
			} else {
				// error other than "not found"
				Ext.Msg.alert('Error',"Error updating task: " + data);
			}
			return;
		}
		
		//Store the timestamp of the current value
		this.timestamp = data.timestamp;
		//Update properties
		this.properties = data.task;
		//Update header
		this.getComponent(0).setContent(this.taskId, data.task.taskProperties.taskDescription.title, data.task);
		//Update title
		this.updateTitle(data.task.taskProperties.taskDescription.title);
		//Update content
		this.updateContent(data.tui);
	
		//Enable download / preview links
		itasks.ttc.common.attachDocumentLinkInformation();
		
		if (data.warning)
			Ext.Msg.alert("Warning",data.warning);
	},
	updateTitle: function(subject) {
		this.setTitle(Ext.util.Format.ellipsis(subject,10));
	},
	updateContent: function(content) {
		var ct = this.getComponent(1);

		if(ct.items.getCount() > 0) {
			//Recursively update tab content
			var cur = ct.getComponent(0);
			
			if(!Ext.isDefined(content.xtype) || cur.xtype == content.xtype) {
				cur.update(content);
			} else {
				// fire done event because subprocesses of parallel possibly also stopped
				if (cur.xtype == 'itasks.ttc.parallel')
					this.fireEvent("taskDone");
					
				ct.remove(0,true);
				ct.add(content);
				ct.doLayout();
			}
		} else {
			//Build initial content
			content.fadeIn = true;
			ct.add(content);
			ct.doLayout();
		}	
		
		//If the toplevel component has a menu, add it to the toolbar
		var comp = ct.getComponent(0);
		if (Ext.isDefined(comp)){
			var tbar = ct.getTopToolbar();
			while(tbar.items.getCount() > 1) {
				tbar.remove(tbar.get(1));
			}
			var menu = comp.menu;
			if(menu) {
				for(var i = 0; i < menu.length; i++) {
					tbar.add(menu[i]);
				}
				tbar.doLayout();
			}
		}
	},
	cancel: function(){
		var me = this;
		
		var doCancel = function(btn){	
			if(btn == "yes"){
				var url = itasks.config.serviceUrl + "/json/tasks/" + me.taskId + "/cancel";
				var params = {};
				var cb = function(data){
					if(data.success)
						me.fadeOut();
					else
						Ext.Msg.alert('Error','Failed to cancel task: '+data.error);
				};		
				me.remoteCall(url,params,cb);	
			}
		}
		
		Ext.Msg.confirm("Cancel Task","Are you sure you wish to cancel this task?",doCancel);
	},
	showProperties: function(){
		var p = this.properties;
	
		var w = new Ext.Window({
			title : 'Task Properties',
			iconCls: 'icon-properties',
			resizable: false,
			width: 500,
			height: 300,
			layout: 'accordion',
			cls: 'task-properties-window',
			defaults: {
				bodyStyle: 'padding: 10px'
			},
			layoutConfig: {
				titleCollapse: false,
				hideCollapseTool: true
			},
			items: [
				{ title: 'Runtime Properties'
				, iconCls: 'icon-currentwork'
				, layout: 'form'
				, defaultType: "staticfield"
				, items: [{
					name: "progress",
					fieldLabel: "Progress",
					format: itasks.util.formatProgress,
					value: p.progress
				},{
					name: "priority",
					fieldLabel: "Priority",
					format: itasks.util.formatPriority,
					value: p.managerProperties.priority
				},{
					name: "issuedAt",
					fieldLabel: "Issued at",
					format: itasks.util.formatDate,
					value: p.systemProperties.issuedAt
				},{
					name: "firstEvent",
					fieldLabel: "First worked on",
					format: itasks.util.formatStartDate,
					value: p.systemProperties.firstEvent
				},{
					name: "latestEvent",
					fieldLabel: "Last worked on",
					format: itasks.util.formatStartDate,
					value: p.systemProperties.latestEvent
				}]
				}
			]
		});
		
		w.show();
	},
	
	fadeOut: function() {
		this.fireEvent("taskRedundant");
				
		var ct = this.get(1);
		ct.removeAll();
		ct.add({
			xtype: "itasks.ttc.finished",
			subject: "Task completed",
			description: "This task is completed or it's completion is no longer required.<br />Thank you for your effort.",
			destroyCmp: this
		});
		this.doLayout();
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
			
			var subject = subject + (itasks.config.debug ? (" (" + taskid + ")") : "");
			
			this.body.update( String.format(
				'<div class="worktab-header {1}">'+
					'<div class="worktab-header-status {0}"></div><div class="worktab-header-separator"></div><div class="worktab-header-text">'+
						'<table><tr><th>Subject:</th><td>{2}</td><th>Managed by:</th><td>{3}</td><th>Deadline:</th><td>{4}</td></table>'+
					'</div>'+
				'</div>'+
				'<div class="worktab-header-indicator">'
				, worktabStatus(properties.progress),worktabBackground(properties.managerProperties.priority),subject, itasks.util.formatUser(properties.systemProperties.manager)
				, itasks.util.formatDeadline(properties.managerProperties.deadline)
				));
	},
	setBusy: function(busy) {
		var indicator = this.getEl().child(".worktab-header-indicator");
		if(indicator)
			indicator.setVisible(busy);
	}	
});

Ext.reg("itasks.work",itasks.WorkPanel);
Ext.reg("itasks.work-header",itasks.WorkHeaderPanel);