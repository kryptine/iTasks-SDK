Ext.define('itwc.container.Viewport',{
	extend: 'Ext.container.Viewport',
	layout: 'fit',
	initComponent: function() {
		this.callParent(arguments);
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
			} else if (step[0] === "w") {
				//Step for windows... TODO
				return undef;
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
