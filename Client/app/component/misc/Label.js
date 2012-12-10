Ext.define('itwc.component.misc.Label',{
	extend: 'Ext.form.Label',
	alias: 'widget.itwc_label',
	
	previous: false,
	
	initComponent: function() {
		var me = this;
		
		me.text = me.value;
		me.callParent(arguments);
	},
	afterRender: function() {
		var next;
		this.callParent(arguments);
		
		//Make this label target the next component
		if(target = this.previous ? this.previousSibling() : this.nextSibling()) {
				this.forId = target.getId() + '-inputEl';
				this.el.set({for: this.forId});
		}
	}
});
