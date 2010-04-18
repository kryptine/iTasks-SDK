Ext.ns('itasks.ttc');

itasks.ttc.MonitorContainer = Ext.extend(Ext.Panel,{
	initComponent: function() {
		Ext.apply(this,{
			defaults: {
				unstyled: true
			},
			bodyStyle: 'padding: 10px',
			unstyled: true,
			cls: 'MonitorContainer',
			items: [
				{ xtype: 'itasks.ttc.common.description'
				, cls: 'MonitorContainerDescription'
				, description: 'Monitor task'
				, headerButton: this.headerButton
				},
				{ xtype: 'panel'
				, cls: 'MonitorPanel'
				, unstyled: true
				, html: this.html
				}
			]
		});
		delete this.html;
		
		itasks.ttc.MonitorContainer.superclass.initComponent.apply(this,arguments);
	},
	
	update: function(data){		
		if(this.get(1).rendered){
			this.get(1).el.update(data.html);
		}else{
			this.get(1).html = data.html;
		}
	}
});

Ext.reg('itasks.ttc.monitor',itasks.ttc.MonitorContainer);