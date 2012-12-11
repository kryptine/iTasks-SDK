Ext.define('itwc.component.choice.Dropdown',{
	extend: 'Ext.form.field.ComboBox',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_dropdown',
	triggerAction: 'all',
	forceSelection: true,
	
	initComponent: function() {
		var me = this,
			store = [],
			numOptions = me.options.length, i;
		
		for(i=0; i < numOptions; i++) {
			store[store.length] = [i, me.options[i]];
		}
		
		me.store = store;
		
		if(Ext.isArray(me.value)) {
			me.value = me.value.length ? store[me.value[0]] : "";
		} else if(Ext.isNumber(me.value) && me.value >= 0 && me.value < numOptions) {
			me.value = store[me.value][0];
		} else {
			me.value = null;
		}

		me.callParent(arguments);
		me.initEditable();
	} 
});
