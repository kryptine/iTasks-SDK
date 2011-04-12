Ext.ns("itasks.tui");

itasks.tui.HtmlContainer = itasks.tui.extendBase(Ext.Panel, {
	initComponent: function() {
		this.html = this.value;
		this.unstyled = true;
		this.border = false;
		this.listeners = {};
				
		itasks.tui.base.initComponent.call(this,arguments);
	},
	setValue: function(value) {
		this.update(value);
	}
});

Ext.reg("itasks.tui.Html",itasks.tui.HtmlContainer);
