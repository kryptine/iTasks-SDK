Ext.define('itasks.component.Choice',{
	extend: 'Ext.form.CheckboxGroup',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks.choice',
	columns: 1,
	initComponent: function() {
		//Names of checkbox groups have to be unique, so use data path field for events
		this.editName = this.name;
		this.name = this.getId();
		
		if(!this.width)
			this.width = 300;
			this.hwrap = true;
			this.simulatedWidth = true;
		if(!this.height) {
			this.height = 300;
			this.vwrap = true;
			this.simulatedHeight = true;
		}

		var selection = Ext.isArray(this.value) ? this.value : [];
		this.checkRadio = selection.length == 0;
		var isSelected = function(idx){
			for(var i=0; i<selection.length; i++){
				if(idx == selection[i]) return true;
			}
			return false;
		};
		
		this.callParent(arguments);
			
		for(var i=0; i < this.options.length; i++) {
			if(this.allowMultiple){
				this.add({
					xtype: 'checkbox',
					boxLabel: this.options[i],
					id: this.id+'-cb-'+i,
					name: 'sel-'+i,
					value: i,
					checked: isSelected(i)
				});
			} else {
				this.add({
					xtype: 'radio',
					boxLabel: this.options[i],
					name: this.name,
					value: i,
					checked: isSelected(i)
				});
			}
		}
	},
	getEditName: function() {
		return this.editName;
	},
	getValue : function(){
		var checked = [];
		var multiple = this.allowMultiple;
			
		var i = 0, len = this.items.length;
		for(i =0; i < len; i++) {
			if(this.items.get(i).checked) {
				if(multiple)
					checked.push(i);
				else
					return [i];
			}
		}
		return checked;
	}
});
