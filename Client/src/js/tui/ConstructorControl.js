Ext.ns('itasks.tui');

itasks.tui.ConstructorControl = Ext.extend(Ext.Panel,{
	
	initComponent : function(){
		
		//this.title = "Constructor Test";
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		//this.width =  '100%';
		this.unstyled = true;
		
		var store = [["","Select..."]];
		
		for(var i=0; i<this.consValues.length; i++){
			store[store.length] = [this.consValues[i],this.consValues[i]];
		}
		
		if(!this.staticDisplay){
			this.consField = new Ext.form.ComboBox({
				name: this.name+'c',
				id: this.id+'c',
				triggerAction: 'all',
				editable: false,
				store: store,
				value: store[(this.consSelIdx+1)][1],
				hideLabel: true,
				style: 'margin-bottom: 4px'
			});
		}else{
			this.consField = {
				id : this.id+'c',
				staticDisplay: true,
				html: store[(this.consSelIdx+1)][1],
				hideLabel: true,
				unstyled: true,
				bodyStyle: 'margin-bottom: 4px'
			}
		}
		
		this.consField.setValue = function(value){
				if(this.staticDisplay){
					this.update(value);
				}else{
					Ext.form.ComboBox.superclass.setValue.call(this,value);
				}
		}
		
		this.items = [this.consField].concat(this.items);	
	
		itasks.tui.ConstructorControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.tui.Constructor',itasks.tui.ConstructorControl);