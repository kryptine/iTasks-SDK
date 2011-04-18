Ext.ns('itasks.tui');

itasks.tui.FormContainer = Ext.extend(Ext.Container,{
	unstyled: true,
	layout: 'form',
	initComponent: function(){
		if(this.fieldLabel == null) {
			delete this.fieldLabel;
		} else {
			this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		}	
		itasks.tui.FormContainer.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.tui.FormContainer',itasks.tui.FormContainer);
