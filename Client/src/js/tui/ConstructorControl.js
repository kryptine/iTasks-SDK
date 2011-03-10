Ext.ns('itasks.tui');

itasks.tui.ConstructorControl = Ext.extend(Ext.Panel,{
	
	initComponent : function(){

		if(this.fieldLabel == null)
			delete this.fieldLabel;
		else
			this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.unstyled = true;
		this.autoHeight = true;
				
		var store = [["","Select..."]];
		
		for(var i=0; i<this.consValues.length; i++){
			store[store.length] = [this.consValues[i],this.consValues[i]];
		}
		
		if(!this.staticDisplay){
			this.consField = new Ext.form.ComboBox({
				name: this.name,
				id: this.id + 'c',
				triggerAction: 'all',
				editable: false,
				store: store,
				value: store[(this.consSelIdx+1)][1],
				hideLabel: true,
				style: 'margin-bottom: 4px',
				msgTarget: 'side',
				valueNotFoundText: 'Value not found...',
				listeners: {select: {fn: this.onChange, scope: this}}
			});
		}else{
			this.consField = {
				id : this.id + 'c',
				staticDisplay: true,
				html: store[(this.consSelIdx+1)][1],
				hideLabel: true,
				unstyled: true,
				bodyStyle: 'margin-bottom: 4px'
			}
		}
		
		this.consField.setValue = function(value){
			if(this.staticDisplay){
				if(value == "") value = "Select...";				
				if(this.el) this.el.dom.innerHTML = value;
			}else{
				Ext.form.ComboBox.superclass.setValue.call(this,value);
				if(value == "" ) this.setRawValue("Select...");
			}
		};
		//Copy the items into another array
		var panelItems = this.items.slice(0);
		delete this.items;
	
		this.itemPanel = new Ext.Panel(
		{ layout: 'form'
		, autoHeight: true
		, items: panelItems
		, frame: true
		, baseCls: 'x-constructor-panel'
		, hidden: panelItems.length == 0	//Initially hide the itemPanel if there are no items 
		});
		
		this.items = [this.consField, this.itemPanel];	
		this.oldConsIdx = this.consSelIdx + 1;
		
		itasks.tui.ConstructorControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		var newConsIdx = this.consField.selectedIndex;

		// only generate change event if different cons is chosen
		if (newConsIdx != this.oldConsIdx){
			this.oldConsIdx = newConsIdx;
			this.fireEvent('tuichange',this.name,this.consField.getValue());
		}
	},
	afterRender: function(){
		itasks.tui.ConstructorControl.superclass.afterRender.call(this,arguments);
	
		//Redirect addition method to the child panel
		this.add = function(c) {
			return this.itemPanel.add(c);
		}
	
		if(this.errorMsg)
			itasks.tui.common.markError(this.consField,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this.consField,this.hintMsg);
	},
	
	setError: function(msg){
		if(this.staticDisplay)
			return;
	
		if(msg == "")
			itasks.tui.common.clearError(this.consField);
		else
			itasks.tui.common.markError(this.consField,msg);
	},
	setHint: function(msg){
		if(this.staticDisplay)
			return;
		
		if(msg == "")
			itasks.tui.common.clearHint(this.consField);
		else
			itasks.tui.common.markHint(this.consField,msg);
	}
});

Ext.reg('itasks.tui.Constructor',itasks.tui.ConstructorControl);