Ext.ns('itasks.tui');

itasks.tui.RecordContainer = itasks.tui.extendContainer(Ext.form.FieldSet,{
	defaultWidth: ['FillParent',1,'ContentSize'],
	defaultHeight: ['Wrap'],
	initComponent : function(){
		if(this.title == null)
			delete this.title
	
		delete this.fieldLabel;
		
		this.checkboxName  = this.name+'-cb';
		this.checkboxToggle = this.optional;
		
		itasks.tui.container.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	
	afterRender: function(){
		this.extSuperclass.afterRender.apply(this,arguments);
		
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
	
		this.fireEvent('tuichange',this.taskId,this.name,(this.checkbox.dom.checked ? 'create' : null));
	},
	getMarginsW: function() {
		var el = this.getResizeEl();
        return el.getMargins('lr');
	},
	getMarginsH: function() {
		var el = this.getResizeEl();
        return el.getMargins('tb');
	}
});

Ext.reg('itasks.tui.Record',itasks.tui.RecordContainer);