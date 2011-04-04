Ext.ns("itasks.tui");

itasks.tui.HtmlContainer = Ext.extend(Ext.Panel, {
	initComponent: function() {
		this.html = this.value;
		this.unstyled = true;
		this.border = false;
				
		itasks.tui.HtmlContainer.superclass.initComponent.apply(this,arguments);
	},
	setValue: function(value) {
		this.update(value);
	}
});

Ext.reg("itasks.tui.Html",itasks.tui.HtmlContainer);