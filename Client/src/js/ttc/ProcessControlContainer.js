Ext.ns('itasks.ttc');

itasks.ttc.ProcessControlContainer = Ext.extend(itasks.ttc.TTCBase,{
	
	initComponent: function() {
		this.cls = 'TTCProcessControlContainer';
		
		this.subject = this.properties.managerProperties.subject;
		this.description = "Waiting for <i>" +  this.properties.managerProperties.subject + "</i>";
	
		itasks.ttc.ProcessControlContainer.superclass.initComponent.apply(this,arguments);
	},
	buildComponents: function(data) {	
		this.interactionpanel =
		    { xtype: 'panel'
			, unstyled: true
			, layout: "form"
			, defaultType: "staticfield"
			, cls: "TTCProcessControlPanel"
			, width: 720
			, items: [{
					xtype: "itasks.tui.Username",
					preventMark: true,
					fieldLabel: "Assigned to",
					value: itasks.util.formatUser(this.properties.managerProperties.worker),
					listeners: {change: {fn: this.onWorkerChange, scope: this}}
				},{
					xtype: "itasks.priority",
					fieldLabel: "Priority",
					value: this.properties.managerProperties.priority, 
					listeners: {change: {fn: this.onPriorityChange, scope: this}}
				},{
					fieldLabel: "Progress",
					format: itasks.util.formatProgress,
					value: this.properties.workerProperties.progress
				},{
					fieldLabel: "Issued at",
					format: itasks.util.formatDate,
					value: this.properties.systemProperties.issuedAt
				},{
					fieldLabel: "First worked on",
					format: itasks.util.formatStartDate,
					value: this.properties.systemProperties.firstEvent
				},{
					fieldLabel: "Last worked on",
					format: itasks.util.formatStartDate,
					value: this.properties.systemProperties.latestEvent
				}
			]};
	},
	onWorkerChange: function(fld,nv,ov) {
	
		var ct = this.findParentByType(itasks.WorkPanel);
		var url = itasks.config.serviceUrl+'/json/tasks/'+this.properties.systemProperties.taskId+ '/managerProperties/worker';
		
		var cb = function(response,success) {
			if(!response.success || !success) {
				Ext.Msg.alert("Error","An error has occurred: "+ response.error);
			}else{
				ct.fireEvent("propertyChanged");
			}
		}									
		ct.remoteCall(url,{update: Ext.encode(nv)},cb);	
	},
	onPriorityChange: function(fld,nv,ov) {

		var ct = this.findParentByType(itasks.WorkPanel);
		var url = itasks.config.serviceUrl+'/json/tasks/'+this.properties.systemProperties.taskId+ '/managerProperties/priority';
		
		var cb = function(response,success){
			if(!response.success || !success) {
				Ext.Msg.alert("Error","An error has occurred: "+ response.error);
			}else{
				ct.fireEvent("propertyChanged");
			}
		}									
		ct.remoteCall(url,{update: Ext.encode(nv)},cb);	
	},
	update: function (data) {
		this.properties = data.properties;

		//Update form	
		var p = data.properties;
		var props = [p.managerProperties.worker,p.managerProperties.priority
					,p.workerProperties.progress,p.systemProperties.issuedAt
					,p.systemProperties.firstEvent,p.systemProperties.latestEvent];
		
		
		this.interactionpanel.items.each(function(cmt,i) {
			cmt.setValue(props[i]);
		});
	}
});

Ext.ns('itasks.ttc.process');

itasks.form.PriorityField = Ext.extend(Ext.form.ComboBox,{
	store: [["HighPriority","High"],["NormalPriority","Normal"],["LowPriority","Low"]],
	triggerAction: 'all',
	editable: false,
	forceSelection: true,
	format: itasks.util.formPriority
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

Ext.reg("itasks.priority",itasks.form.PriorityField);
Ext.reg("itasks.progress",itasks.form.ProgressField);
Ext.reg('itasks.ttc.proc-control',itasks.ttc.ProcessControlContainer);