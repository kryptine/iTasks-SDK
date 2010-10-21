Ext.ns("itasks.tui");

itasks.tui.HtmlContainer = Ext.extend(Ext.Panel,{

	initComponent: function() {
	
		this.unstyled = true;
		this.border = false;
		this.hideLabel = true;

		itasks.tui.HtmlContainer.superclass.initComponent.apply(this,arguments);
	},
	update: function(data) {
		console.log("UPDATING");
	}
});

Ext.reg("itasks.tui.Html",itasks.tui.HtmlContainer);