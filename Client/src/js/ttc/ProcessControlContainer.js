Ext.ns('itasks.ttc');

itasks.ttc.ProcessControlContainer = Ext.extend(Ext.Panel,{
	initComponent: function(){
		
		Ext.apply(this,
		{ unstyled: true
		, cls: 'ProcessControlContainer'
		, description: 'Control process properties'
		, items: [
			{ xtype: 'itasks.ttc.common.description'
			, cls: 'ProcessControlDescription'
			, description: 'Control process properties'
			, headerButton: this.headerButton
			, width: 720
			},
			{ xtype: 'panel'
			, unstyled: true
			, layout: "form"
			, defaultType: "staticfield"
			, cls: 'ProcessControlPanel'
			, width: 720
			, items: [
				{ html: "Waiting for <i>" +  this.properties.managerProperties.subject + "</i>"
				, style: "margin: 0px 0px 20px 0px;"
				, unstyled: true			
				, xtype: 'panel'
				},{
					xtype: "panel",
					layout: "form",
					defaultType: "staticfield",
					unstyled: true,
					items: [{
						xtype: "itasks.tui.Username",
						preventMark: true,
						fieldLabel: "Assigned to",
						value: this.properties.managerProperties.worker,
						listeners: {
							"change" : { fn: 
								function(fld,nv,ov) {
									var ct = this.findParentByType(itasks.WorkPanel);
									var url = itasks.config.serviceUrl+'/json/tasks/'+this.properties.systemProperties.taskId+ '/managerProperties/worker';
									var upd = (nv != "")?'["NamedUser",'+Ext.encode(nv)+']':nv;
									
									var cb = function(response,success){
										if(!response.success || !success) {
											Ext.Msg.alert("Error","An error has occurred: "+response.error);
										}else{
											ct.fireEvent("propertyChanged");
										}
									}									
									
									ct.remoteCall(url,{update: upd},cb);
								},
								scope: this 
							}
						}
					},{
						xtype: "itasks.priority",
						fieldLabel: "Priority",
						value: this.properties.managerProperties.priority, 
						listeners: {
							"change" : { fn: 
								function(fld,nv,ov) {
									var ct = this.findParentByType(itasks.WorkPanel);
									var url = itasks.config.serviceUrl+'/json/tasks/'+this.properties.systemProperties.taskId+ '/managerProperties/priority';
									var upd = Ext.encode(nv);
									
									var cb = function(response,success){
										if(!response.success || !success) {
											Ext.Msg.alert("Error","An error has occurred: "+response.error);
										}else{
											ct.fireEvent("propertyChanged");
										}
									}									
									
									ct.remoteCall(url,{update: upd},cb);
								},
								scope: this 
							}
						}
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
				]}
			]}
		]});
		
		itasks.ttc.ProcessControlContainer.superclass.initComponent.apply(this,arguments);	
	},
	
	update: function (data) {
		this.properties = data.properties;

		var p = data.properties;
		var props = [p.managerProperties.worker,p.managerProperties.priority,p.workerProperties.progress,p.systemProperties.issuedAt,p.systemProperties.firstEvent,p.systemProperties.latestEvent];
				
		if(this.getComponent(1).getComponent(0).body) this.getComponent(1).getComponent(0).body.update("Waiting for <i>" + Ext.util.Format.htmlEncode(p.managerProperties.subject) + "</i>");
		this.getComponent(1).getComponent(1).items.each(function(cmt,i){ cmt.setValue(props[i]); });
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