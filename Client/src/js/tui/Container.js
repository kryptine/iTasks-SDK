Ext.ns('itasks.tui');

itasks.tui.Container = Ext.extend(Ext.Container,{

	initComponent: function(){
		this.unstyled = true;
		this.autoHeight = true;
		
		if(this.restrictedWidth)
			this.width = 700;

		if(this.fieldLabel == null) {
			delete this.fieldLabel;
		} else {
			this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		}	
		itasks.tui.Container.superclass.initComponent.apply(this,arguments);
	},
	afterRender: function(){
		itasks.tui.Container.superclass.afterRender.call(this,arguments);
	}
});

Ext.reg('itasks.tui.Container',itasks.tui.Container);