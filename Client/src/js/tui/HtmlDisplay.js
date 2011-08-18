Ext.ns("itasks.tui");

itasks.tui.HtmlContainer = itasks.tui.extendControl(Ext.Container, {
	style: 'white-space: nowrap',
	
	initComponent: function() {
		if(!this.height)
			this.vwrap = true;
		if(!this.width)
			this.hwrap = true;
			this.hflex = 1;
	
		this.html = this.value;
		this.unstyled = true;
		this.border = false;
		this.listeners = {};
				
		itasks.tui.control.initComponent.apply(this,arguments);
	},
	afterRender: function() {
		this.contentSize = Ext.util.TextMetrics.measure(this.el.dom,this.value);
		if (this.tooltip) {
			new Ext.ToolTip({html: this.tooltip, target: this.getEl()});
		}
		
		this.extSuperclass.afterRender.apply(this,arguments);
	},
	setValue: function(value) {
		itasks.tui.forceLayout = true;
		this.setWidth('auto');
		this.setHeight('auto');
		this.update(value);
	},
	
	getContentWidth: function() {
		return this.contentSize.width;
	},
	getContentHeight: function() {
		return this.contentSize.height;
	}
});

Ext.reg("itasks.tui.Html",itasks.tui.HtmlContainer);
