Ext.ns('itasks.ttc');

itasks.ttc.ResultContainer = Ext.extend(Ext.Panel,{
	initComponent: function() {
		Ext.apply(this,{
			defaults: {
				unstyled: true
			},
			bodyStyle: 'padding: 10px',
			unstyled: true
		});
		
		itasks.ttc.ResultContainer.superclass.initComponent.apply(this,arguments);
	},
	
	update: function(data){		
		if(this.rendered){
			this.el.update(data.html);
		}else{
			this.html = data.html;
		}
	}
});

Ext.reg('itasks.ttc.result',itasks.ttc.ResultContainer);