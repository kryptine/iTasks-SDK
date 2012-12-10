Ext.define('itwc.component.choice.RadioGroup',{
	extend: 'itwc.container.Container',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_radiogroup',
	
	initComponent: function() {
		//Setup options
		var me = this,
			items = [],
			options = me.options,
			numOptions = options.length,
			choice, i;
			
		choice = Ext.isNumber(me.value) ? me.value : -1;
		
		for(i = 0; i < numOptions; i++) {
			
			items[i] =	{xtype:"itwc_container"
						,direction: "horizontal"
						,valign: "middle"
						,margins: "0 0 2 0"
						,items: [
							{xtype:"radio"
							,name: me.id + "-option"
							,inputValue: i
							,checked: choice == i
							,listeners: {
								change: { fn: me.onRadioChange, scope: me}
								}
							,margins: "0 5 0 0"
							},
							{xtype:"itwc_label",value: options[i], previous: true}
							]
						}	
		}
		me.items = items;
		
		me.callParent(arguments);
		me.initEditable();
	},
	onRadioChange: function(field,nVal,oVal) {
		var me = this;
		
		if(nVal) {
			me.value = field.inputValue;
			me.fireEvent('change',me.value);
		}
	},
	getValue: function() {
		return this.value;
	},
	setEditValue: function(value) {
		this.value = value;
	}
});