Ext.ns("itasks.tui");

itasks.tui.HtmlContainer = itasks.tui.extendControl(Ext.Container, {
	style: 'white-space: nowrap',
	defaultWidth: ['FillParent',1,'ContentSize'],
	defaultHeight: ['Wrap'],
	initComponent: function() {
		this.html = this.value;
		this.unstyled = true;
		this.border = false;
		this.listeners = {};
				
		itasks.tui.control.initComponent.apply(this,arguments);
	},
	setValue: function(value) {
		this.setWidth('auto');
		this.setHeight('auto');
		this.update(value);
	}
});

Ext.reg("itasks.tui.Html",itasks.tui.HtmlContainer);
