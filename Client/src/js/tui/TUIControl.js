Ext.ns("itasks.tui");

itasks.tui.extendControl = function(extSuper,overrides) {
	return itasks.util.extend(extSuper,itasks.tui.control,overrides);
};

itasks.tui.control = Ext.apply(itasks.util.clone(itasks.tui.base),{
	allowBlank:	true,
	enableKeyEvents: true,
	initComponent: function() {
		if (!this.margins) {
			this.margins =	{ left		: 3
							, top		: 3
							, right		: 3
							, bottom	: 3
							};
		}
	
		if (!Ext.isDefined(this.listeners)) {
			this.listeners = {change : {fn: this.onChange, scope: this}};
			
			if (Ext.isNumber(itasks.config.eventTimerInterval)) {
				this.listeners.keypress = {fn: this.onKeypress, scope: this};
				this.delayedSendEvent = new Ext.util.DelayedTask(this.onChange,this);
			}
		}
		
		itasks.tui.base.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		if (this.delayedSendEvent) {
			this.eventTimerActive = false;
			this.delayedSendEvent.cancel();
		}
		
		if(this.isValid()) {
			var v = this.getValue();
			v = v === "" ? null : v;
			if (v !== this.lastEventValue) {
				this.fireEvent('tuichange',this.taskId,this.name,v);
			}
			this.lastEventValue = v;
		}
	},
	onKeypress: function() {
		if (!this.eventTimerActive) {
			this.eventTimerActive = true;
			this.delayedSendEvent.delay(itasks.config.eventTimerInterval);
		}
	}
});