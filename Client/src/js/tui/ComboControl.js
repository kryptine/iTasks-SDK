Ext.ns('itasks.tui');

itasks.tui.ComboControl = itasks.tui.extendControl(Ext.form.ComboBox,{
	width: 330,
	height: 25,
	triggerAction: 'all',
	forceSelection: true,
	
	initComponent: function() {
		var store = [];
		for(var i=0; i<this.options.length; i++) {
			store[store.length] = [i,this.options[i]];
		}
		
		this.store = store;
		this.value = Ext.isNumber(this.value) ? store[this.value][1] : null;
		this.listeners = {select: this.onChange, change: this.onChange};
		this.oldConsIdx = this.value;
		
		itasks.tui.control.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.tui.Combo',itasks.tui.ComboControl);