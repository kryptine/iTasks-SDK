Ext.ns('itasks.tui');

itasks.tui.ConstructorControl = Ext.extend(Ext.Panel,{
	
	initComponent : function(){

		if(this.fieldLabel == null) delete this.fieldLabel;
		else this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
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
				valueNotFoundText: 'Value not found...'
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
		
		var panelItems = this.items.slice(0); //copy the items into another array
		delete this.items;
		
		this.itemPanel = new Ext.Panel(
		{ layout: 'form'
		, autoHeight: true
		, items: panelItems
		, frame: true
		, baseCls: 'x-constructor-panel'
		});
		
		this.itemPanel.on('add',function(){ this.showOrHide(); },this.itemPanel);
		this.itemPanel.on('remove', function(){ this.showOrHide();  },this.itemPanel);
		
		this.itemPanel.showOrHide = function(){			
			(function() {
				if((this.items && this.items.length > 0)){
					this.show();
				}else{
					this.hide();
				}
			}).defer(50,this);
		};		
		
		this.items = [this.consField, this.itemPanel] //[this.consField].concat(this.items);	
			
		itasks.tui.ConstructorControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.ConstructorControl.superclass.afterRender.call(this,arguments);
	
		(function(){
			this.itemPanel.showOrHide();			
			
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
			
			this.add = function(obj){
				return this.itemPanel.add(obj);
			}
		}).defer(50,this);
	},
	
	setError: function(msg){
		if(this.staticDisplay) return;
		
		(function(){
			if(msg == "") itasks.tui.common.clearError(this.consField)//this.itemPanel.clearError();
			else itasks.tui.common.markError(this.consField,msg);
			//this.itemPanel.showOrHide();
		}).defer(50,this);
	},
	
	setHint: function(msg){
		if(this.staticDisplay) return;
		
		(function(){
			if(msg == "") itasks.tui.common.clearHint(this.consField);
			else itasks.tui.common.markHint(this.consField,msg);
			//this.itemPanel.showOrHide();
		}).defer(50,this);
	}
});

Ext.reg('itasks.tui.Constructor',itasks.tui.ConstructorControl);