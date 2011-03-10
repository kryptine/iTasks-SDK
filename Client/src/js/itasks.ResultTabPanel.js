/**
* Tab panel which shows the result of a task
*/

Ext.ns("itasks");

//services/json/tasks/<taskId>/result/tui

itasks.ResultPanel = Ext.extend(itasks.RemoteDataPanel, {

	initComponent : function(){
		Ext.apply(this, {
			title: 'Loading result...',
			closable: true,
			autoDestroy: true,
			iconCls: 'icon-task-result',
			layout: 'border',
			url: itasks.config.serviceUrl+ "/json/tasks/" + this.taskId+ "/result/tui",
			params: {},
			items: [{
				xtype: 'itasks.result-header',
				region: 'north',
				height: 25
			},{
				xtype: 'panel',
				region: 'center',
				layout: 'anchor',
				ctCls: 'worktab-container',
				bodyStyle: 'background-color: #eee',
				defaults: { anchor: 'bottom 0' }
			}]
		});
		
		itasks.ResultPanel.superclass.initComponent.apply(this,arguments);
		
		this.on("remoteCallStart",function() { this.getComponent(0).setBusy(true); },this);
		this.on("remoteCallEnd",function() { this.getComponent(0).setBusy(false); },this);
	},
	
	update: function(data){
		var rh = this.getComponent(0);
		var rp = this.getComponent(1);
		
		var props = data.task;
		var subject = props.taskProperties.taskDescription.title;
				
		this.properties = props;
		this.setTitle(Ext.util.Format.ellipsis(subject,10));		
				
		rh.setContent(data.tui.taskId, subject, props);
		
		if(rp.initialized){
			rp.removeAll();
			rp.doLayout();
		}
		
		rp.add(data.tui);
		rp.doLayout();
		rp.initialized = true;
	}
});

itasks.ResultHeaderPanel = Ext.extend(Ext.Panel, {
				
	initComponent: function() {
		Ext.apply(this, {
			deferredRender: false,
			html: "Loading...",
			baseCls: "worktab-header-normal-priority"
		});
		itasks.ResultHeaderPanel.superclass.initComponent.apply(this,arguments);
		
	},
	setContent: function(taskid, subject, properties) {						
			var subject = subject + (itasks.config.debug ? (" (" + taskid + ")") : "");
			
			this.body.update( String.format(
				'<div class="worktab-header">'+
					'<div class="worktab-header-text">'+
						'<table><tr><th>Result of:</th><td>{0}</td><th>Provided by:</th><td>{1}</td></table>'+
					'</div>'+
				'</div>'+
				'<div class="worktab-header-indicator">'
				, subject, properties.managerProperties.worker
				));
	},
	setBusy: function(busy) {
		var indicator = this.getEl().child(".worktab-header-indicator");
		if(indicator)
			indicator.setVisible(busy);
	}	
});

Ext.reg("itasks.result",itasks.ResultPanel);
Ext.reg("itasks.result-header",itasks.ResultHeaderPanel);