Ext.define('itwc.component.view.HTML',{
	extend: 'Ext.Component',
	alias: 'widget.itwc_view_html',
    mixins: ['itwc.Sizeable'],
	autoScroll: true,

	initComponent: function() {
		this.html = this.value;	
        this.initSize();
		this.callParent(arguments);
	},
	setValue: function(value) {
		if(this.rendered) {
			this.el.update(value);
			if(this.ownerCt) {
				this.ownerCt.doLayout();
			}
		} else {
			this.value = value;
		}
	}
});
