Ext.define('itasks.component.show.Currency',{
	extend: 'Ext.Component',
	alias: 'widget.itasks_show_currency',
	initComponent: function() {
		this.html = Ext.util.Format.currency(this.value[1] / 100.0, this.sign(this.value[0]));
	},
	sign: function (cur) {
		switch(cur) {
			case 'EUR': return '&euro;';
			case 'USD': return '$';
			default: return '';
		}
	}
});