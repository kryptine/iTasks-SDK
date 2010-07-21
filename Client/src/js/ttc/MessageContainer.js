Ext.ns('itasks.ttc');

itasks.ttc.MessageContainer = Ext.extend(itasks.ttc.InteractionBase, {

	initComponent : function() {
		this.cls = 'MessageContainer';
		
		itasks.ttc.MessageContainer.superclass.initComponent.apply(this,arguments);
	},
	
	buildComponents: function(data){
		this.panel = {
			xtype: 'itasks.ttc.message.panel',
			items: data.content.form,
			buttons: data.content.buttons
		}
		
		this.descpanel = {
			xtype: 'itasks.ttc.common.description',
			cls: 'MessageDescription',
			description: data.description,
			headerButton: this.headerButton,
			width: 720
		}
	},
	
	update: function(data){
		//this.setupHotkeys(data.hotkeys);
	}
});

Ext.ns('itasks.ttc.message');

itasks.ttc.message.MessagePanel = Ext.extend(Ext.Panel, {

	initComponent : function(){
		Ext.apply(this,
		{ layout: 'fit'
		, unstyled: true
		, cls: 'MessagePanel'
		, width: 720
		});
		
		itasks.ttc.message.MessagePanel.superclass.initComponent.apply(this,arguments);
	}

});

Ext.reg('itasks.ttc.message',itasks.ttc.MessageContainer);
Ext.reg('itasks.ttc.message.panel', itasks.ttc.message.MessagePanel);