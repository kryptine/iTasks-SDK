Ext.define('itwc.component.misc.Label', {
	extend: 'Ext.form.Label',
	alias: 'widget.itwc_label',
    mixins: ['itwc.Sizeable'],
	
	previous: false,
	
	initComponent: function() {
		var me = this;
		
		me.text = me.value;
        me.initSize();
		me.callParent(arguments);
	},
	afterRender: function() {
		var next;
    var props = {};

		this.callParent(arguments);
		
		//Make this label target the next component
    var target = this.previous ? this.previousSibling() : this.nextSibling();

		if (target) {
				this.forId = target.getId() + '-inputEl';
        props["for"] = this.forId;
				this.el.set(props);
		}
	}
});
