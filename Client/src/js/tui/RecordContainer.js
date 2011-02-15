Ext.ns('itasks.tui');

itasks.tui.RecordContainer = Ext.extend(Ext.form.FieldSet,{

	initComponent : function(){
		
		this.autoHeight = true;
		this.boxMinWidth = 500;
		this.autoWidth = true;
		
		if(this.title == null)
			delete this.title
	
		delete this.fieldLabel;
		
		this.checkboxName  = this.name+'-cb';
		this.checkboxToggle = this.optional;
		
		itasks.tui.RecordContainer.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	
	afterRender: function(){
		itasks.tui.RecordContainer.superclass.afterRender.call(this,arguments);
		
		if(this.optional){
			this[this.hasValue?'expand':'collapse']();
			this.checkbox.dom.checked = this.hasValue;
		}
	},
	onCheckClick : function() {
		if(this.checkbox.dom.checked)
			this.expand();
		else
			this.collapse();
	
		this.fireEvent('tuichange',this.name,(this.checkbox.dom.checked ? 'create' : ''));
	}
});

Ext.reg('itasks.tui.Record',itasks.tui.RecordContainer);