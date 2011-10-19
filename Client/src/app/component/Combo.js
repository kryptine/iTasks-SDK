Ext.define('itasks.component.Combo',{
	extend: 'Ext.form.field.ComboBox',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks_combo',
	triggerAction: 'all',
	forceSelection: true,
	
	initComponent: function() {

		var store = [],
					 i,
					 numOptions = this.options.length;
		
		for(i=0; i < numOptions; i++) {
			store[store.length] = [i, this.options[i]];
		}
		
		this.store = store;
		this.value = Ext.isNumber(this.value) ? store[this.value][0] : null;
	
		this.callParent(arguments);
	} 
});
