Ext.ns('itasks.tui');

itasks.tui.RecordContainer = Ext.extend(Ext.form.FieldSet,{

	initComponent : function(){
	
		this.hideLabel = true;
		//this.width =  '100%';
		//this.hideLabel = this.fieldLabel == null;
		//this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.autoHeight = true;
		this.layout = 'form';
		
		if(this.title == null) delete this.title
		else this.title = itasks.util.fieldLabel(this.optional,this.title);
		
		this.checkboxName  = this.name+'-cb';
		this.checkboxToggle = this.optional;
		
		itasks.tui.RecordContainer.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.RecordContainer.superclass.afterRender.call(this,arguments);
		
		if(this.optional){
			this[this.hasValue?'expand':'collapse']();
			this.checkbox.dom.checked = this.hasValue;
		}
	},
	
	onCheckClick : function() {
		this[this.checkbox.dom.checked?'expand':'collapse']();
	
		var formCt = this.findParentByType(itasks.ttc.FormContainer);
		formCt.addUpdate(this.name,(this.checkbox.dom.checked)?'create':'');
		formCt.sendUpdates(false);	
	}
});

Ext.reg('itasks.tui.Record',itasks.tui.RecordContainer);