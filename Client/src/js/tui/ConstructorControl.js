Ext.ns('itasks.tui');

itasks.tui.ConstructorControl = itasks.tui.extendControl(Ext.Panel,{
	defaultWidth: ['WrapContent',0],
	defaultHeight: ['WrapContent',0],
	unstyled: true,
	initComponent: function() {
		var store = [["","Select..."]];
		for(var i=0; i<this.consValues.length; i++) {
			store[store.length] = [this.consValues[i],this.consValues[i]];
		}
		
		this.consField = new Ext.form.ComboBox({
			name: this.name,
			id: this.id + 'c',
			triggerAction: 'all',
			editable: false,
			store: store,
			value: store[(this.value+1)][1],
			hideLabel: true,
			style: 'margin-bottom: 4px',
			valueNotFoundText: 'Value not found...',
			listeners: {select: {fn: this.onChange, scope: this}}
		});
		
		this.consField.setValue = function(value){
			Ext.form.ComboBox.superclass.setValue.call(this,value);
			if(value == "" ) this.setRawValue("Select...");
		};
		
		//Copy the items into another array
		var panelItems = this.items.slice(0);
		delete this.items;
	
		this.itemPanel = new Ext.Panel({
			items: panelItems,
			frame: true,
			baseCls: 'x-constructor-panel',
			hidden: panelItems.length == 0,	//Initially hide the itemPanel if there are no items
			show: function() {}
		});
		
		this.items = [this.consField, this.itemPanel];	
		this.oldConsIdx = this.value + 1;
		
		itasks.tui.control.initComponent.apply(this,arguments);
		
		this.msgTargetField = this.consField;
	},
	onChange: function() {
		var newConsIdx = this.consField.selectedIndex;

		// only generate change event if different cons is chosen
		if (newConsIdx != this.oldConsIdx){
			this.oldConsIdx = newConsIdx;
			itasks.tui.control.onChange.apply(this,arguments);
		}
	},
	afterRender: function() {
		this.extSuperclass.afterRender.apply(this,arguments);
	
		//Redirect addition method to the child panel
		this.add = function(c) {
			return this.itemPanel.add(c);
		}
	},
	getValue: function() {
		return this.consField.getValue();
	},
	isValid: function() {
		return this.consField.isValid();
	}
});

Ext.reg('itasks.tui.Constructor',itasks.tui.ConstructorControl);