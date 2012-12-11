Ext.define('itwc.component.choice.CheckboxGroup',{
	extend: 'itwc.container.Container',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_checkboxgroup',
	
	initComponent: function() {
		//Setup options
		var me = this,
			items = [],
			options = me.options,
			numOptions = options.length,
			choices, i;
			
		me.value = choices = Ext.isArray(me.value) ? me.value : [];
		
		for(i = 0; i < numOptions; i++) {
			
			items[i] =	{xtype:"itwc_container"
						,direction: "horizontal"
						,valign: "middle"
						,margins: "0 0 2 0"
						,items: [
							{xtype:"checkbox"
							,inputValue: i
							,checked: choices.indexOf(i) != -1
							,listeners: {
								change: { fn: me.onCheckChange, scope: me}
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
	onCheckChange: function(field,nVal,oVal) {
		var me = this;
	
		//Update local value
		me.updateChoice(field.inputValue,nVal); 
	
		//Sync change
		me.viewport = me.findViewport();	
		me.viewport.fireEvent('edit',me.taskId,me.getEditorId(),[field.inputValue,nVal]);
	},
	updateChoice: function(i,val) {
		var me = this;
		
		if(val) {
			if(!(i in me.value)) {
				me.value.push(i);
			}
		} else {
			if(i in me.value) {
				me.value = me.value.filter(function(x) {return (x != i);});
			}
		}
	},
	getValue: function() {
		return this.value;
	},
	setEditValue: function(value) {
		this.value = value;
	}
});