Ext.ns("itasks.tui");

itasks.tui.HtmlContainer = Ext.extend(Ext.Panel, {

	initComponent: function() {
		this.unstyled = true;
		this.border = false;
				
		itasks.tui.HtmlContainer.superclass.initComponent.apply(this,arguments);
	},
	setValue: function(value) {
		console.log("SETTING VALUE");
	},
	update: function(data) {
		console.log("UPDATING");
	}
});

Ext.reg("itasks.tui.Html",itasks.tui.HtmlContainer);