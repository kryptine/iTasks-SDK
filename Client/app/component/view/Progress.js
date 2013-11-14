Ext.define('itwc.component.view.Progress',{
	extend: 'Ext.ProgressBar',
	alias: 'widget.itwc_view_progress',
    mixins: ['itwc.Sizeable'],
	
	itwcWidth: 'flex',
	waiting: false,
	
	initComponent: function() {		
		var me = this;
		
        me.initSize();
		if(me.value == 'undetermined') {
			me.value = 0.0;
			me.waiting = true;
		}
		me.callParent(arguments);
	},
	afterRender: function() {
		var me = this;
		
		
		me.callParent(arguments);
		if(me.waiting) {
			me.wait({animate: true, interval: 500, text: me.text});
		}
	},
    setValue: function (value) {
        var me = this;
		if(value == 'undetermined') {
			me.value = 0.0;
			me.waiting = true;
		} else {
            me.value = value;
            if(me.waiting) {
                me.reset();
                me.waiting = false;
            }
        }
        me.updateProgress(me.value);
        if(me.waiting) {
			me.wait({animate: true, interval: 500, text: me.text});
        }
    }
});
