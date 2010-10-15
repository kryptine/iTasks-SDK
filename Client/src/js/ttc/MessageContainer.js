Ext.ns('itasks.ttc');

itasks.ttc.MessageContainer = Ext.extend(itasks.ttc.TTCBase, {

	initComponent : function() {
		this.cls = 'TTCMessageContainer';
		itasks.ttc.MessageContainer.superclass.initComponent.apply(this,arguments);
	},
	buildComponents: function(data){
		this.interactionpanel = {
			xtype: 'panel',
			layout: 'fit',
			cls: 'TTCMessagePanel',
			unstyled: true,
			width: 720,
			items: data.content.form,
			buttons: data.content.buttons
		};		
	}
});

Ext.reg('itasks.ttc.message',itasks.ttc.MessageContainer);