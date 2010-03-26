Ext.ns('itasks.ttc');

itasks.ttc.FinishedContainer = Ext.extend(Ext.Panel,{
	initComponent: function() {
		Ext.apply(this,
		{ unstyled: true
		, cls: 'FinishedContainer'
		, width: 720
		, html: '<div class="FinishedContainer-Text">'+this.msg+'</div>'
		});
		
		itasks.ttc.ResultContainer.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.ttc.finished',itasks.ttc.FinishedContainer);