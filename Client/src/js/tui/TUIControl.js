Ext.ns("itasks.tui");

itasks.tui.extendControl = function(extSuper,overrides) {
	return itasks.util.extend(extSuper,itasks.tui.control,overrides);
};

itasks.tui.control = Ext.apply(itasks.util.clone(itasks.tui.base),{
	allowBlank:	true,

	initComponent: function() {
		if (!this.margins) {
			this.margins =	{ left		: 3
							, top		: 3
							, right		: 3
							, bottom	: 3
							};
		}
	
		this.listeners = this.listeners || {change : {fn: this.onChange, scope: this}};
		
		itasks.tui.base.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		if(this.isValid()) {
			var v = this.getValue();
			this.fireEvent('tuichange',this.taskId,this.name,v === "" ? null : v);
		}
	}
});