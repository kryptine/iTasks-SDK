Ext.ns('itasks.tui');

itasks.tui.FormContainer = itasks.tui.extendContainer(Ext.Container,{
	unstyled: true,
	layout: 'form',
	defaultWidth: ['Fixed',700],
	defaultHeight: ['Wrap'],
	initComponent: function(){
		if(this.fieldLabel == null) {
			delete this.fieldLabel;
		} else {
			this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		}	
		itasks.tui.container.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.tui.FormContainer',itasks.tui.FormContainer);
