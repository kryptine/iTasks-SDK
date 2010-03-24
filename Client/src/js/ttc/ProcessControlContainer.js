Ext.ns('itasks.ttc');

itasks.ttc.ProcessControlContainer = Ext.extend(Ext.Panel,{
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
						xtype: "itasks.userfield",
						fieldLabel: "Assigned to",
						value: this.properties.managerProps.worker,
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
		itasks.ttc.ProcessControlContainer.superclass.initComponent.apply(this,arguments);
	},
	update: function (data) {
		this.properties = data.properties;

		var p = data.properties;
		var props = [p.managerProps.worker,p.managerProps.priority,p.workerProps.progress,p.systemProps.issuedAt,p.systemProps.firstEvent,p.systemProps.latestEvent];
				
		if(this.getComponent(0).body) this.getComponent(0).body.update("Waiting for <i>" + Ext.util.Format.htmlEncode(data.properties.managerProps.subject) + "</i>");
		this.getComponent(1).items.each(function(cmt,i){ cmt.setValue(props[i]); });
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

Ext.reg("itasks.priority",itasks.form.PriorityField);
Ext.reg("itasks.progress",itasks.form.ProgressField);
Ext.reg('itasks.ttc.proc-control',itasks.ttc.ProcessControlContainer);
