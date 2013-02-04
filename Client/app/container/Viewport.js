Ext.define('itwc.container.Viewport',{
	extend: 'Ext.container.Viewport',
	layout: 'fit',
	mixins: ['itwc.container.HotkeyArea'],

	initComponent: function() {
		this.callParent(arguments);
	},
	afterRender: function() {
		var me = this;
		
		me.callParent();
		me.initHotkeys();
	},
	onDestroy: function () {
		var me = this;

		me.destroyHotkeys();
		me.callParent(arguments);
	},
	getComponentByPath: function(path) {
		var me = this,
			steps = path.split('-'),
			numSteps = steps.length,
			cmp = me,
			step, i, undef;
			
		if(path == "") {
			return me;
		}
		for(i = 0; i < numSteps; i++) {
			step = steps[i];
			
			if(step === "m") {
				cmp = cmp.getDockedComponent(0);
				if(!cmp)
					return undef;
			} else if (step === "w") {
				if(i < numSteps - 1) { 
					if(cmp.windows && cmp.windows.length) {
						if((i+1) < numSteps && cmp.windows[parseInt(steps[i+1])]) {
							cmp = cmp.windows[parseInt(steps[i+1])];
							i++;
						}
					} else {
						return undef;
					}
				}
			} else {
				if(cmp.items && cmp.items.get) {
					cmp = cmp.items.get(parseInt(step));
					if(!cmp)
						return undef;
				} else {
					return undef;
				}
			}	
		}
		return cmp;
	}
});
