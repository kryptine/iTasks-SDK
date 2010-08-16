Ext.ns('itasks.ttc');

itasks.ttc.ResultContainer = Ext.extend(Ext.Panel,{
	initComponent: function() {
		Ext.apply(this,
		{ unstyled: true
		, cls: 'ResultContainer'
		, width: 720
		, html: '<div class="TTCDescription">'+this.subject+'</div><div class="ResultContainer-Text">'+this.result+'</div>'
		});
		
		itasks.ttc.ResultContainer.superclass.initComponent.apply(this,arguments);
	},
	
	update: function(data){
		if(this.rendered){
			this.body.html = '<div class="TTCDescription">'+data.subject+'</div><div class="ResultContainer-Text">'+data.result+'</div>'
		}else{
			this.html = '<div class="TTCDescription">'+data.subject+'</div><div class="ResultContainer-Text">'+data.result+'</div>'
		}
	}
});

Ext.reg('itasks.ttc.result',itasks.ttc.ResultContainer);