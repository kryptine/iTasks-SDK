Ext.ns("itasks.tui");

itasks.tui.HiddenControl = Ext.extend(Ext.Component,{
	initComponent: function() {
		
		this.autoCreate = {tag: 'div', html: this.value};
		
		this.hideLabel = true;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.hidden = true;
		
		itasks.tui.HiddenControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Hidden",itasks.tui.HiddenControl);