Ext.define('itasks.component.Choice',{
	extend: 'Ext.form.CheckboxGroup',
	mixins: ['itasks.mixin.Editable','itasks.mixin.Wrappable'],
	alias: 'widget.ichoice',
	columns: 1,
	initComponent: function() {
		//Names of checkbox groups have to be unique, so use data path field for events
		this.editName = this.name;
		this.name = this.getId();
		
		if(!this.width)
			this.hwrap = true;
		if(!this.height)
			this.vwrap = true;

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
	//Overwrite wrap size, because it cannot properly measure the wrap size
	getWrapSize: function() {
		
		var i = 0, len = this.items.length, item, size;
		var wrapWidth = 0, wrapHeight = 0;
		
		for(i = 0; i < len; i++) {
			item = this.items.get(i);
			//TODO: Use real measurements here.
			size = {width: 150, height: 22},
		
			wrapWidth = Math.max(wrapWidth,size.width);
			wrapHeight += size.height;
		}
		return {width: wrapWidth, height: wrapHeight};
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