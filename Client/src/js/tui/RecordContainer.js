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
		if(this.errorMsg)
			this.markError(this.errorMsg);
		else if(this.hintMsg)
			this.markHint(this.hintMsg);
	},
	onCheckClick : function() {
		if(this.checkbox.dom.checked)
			this.expand();
		else
			this.collapse();
	
		this.fireEvent('tuichange',this.name,(this.checkbox.dom.checked ? 'create' : ''));
	},
	setError: function(msg){		
		if(this.staticDisplay)
			return;
		
		if(msg == "")
			this.clearError();
		else
			this.markError(msg);
	},
	setHint: function(msg){
		if(this.staticDisplay)
			return;
		
		if(msg == "")
			this.clearHint();
		else
			this.markHint(msg);
	},
	markHint : function (msg){
		if(this.rendered){
			if(this.errorIcon) {
				this.errorIcon.hide();
			}
			if(!this.hintIcon){
				this.hintIcon = this.el.insertFirst({cls: 'x-record-hint-icon'});	
				this.hintIcon.setVisibilityMode(Ext.Element.DISPLAY);
			}

			this.hintIcon.dom.innerHTML = msg;
			this.hintIcon.setVisible(true);					
		}
	},
	markError : function(msg){
		if(this.rendered){
			if(this.hintIcon){
				this.hintIcon.hide();
			}
			
			if(!this.errorIcon){
				this.errorIcon = this.el.insertFirst({cls: 'x-record-invalid-icon'});
				this.errorIcon.setVisibilityMode(Ext.Element.DISPLAY);
			}
			
			this.errorIcon.dom.innerHTML = msg;
			this.errorIcon.setVisible(true);
		}
	},
	clearHint : function(){
		if(this.hintIcon)
			this.hintIcon.setVisible(false);
	},	
	clearError: function(){
		if(this.errorIcon)
			this.errorIcon.setVisible(false);
	}
});

Ext.reg('itasks.tui.Record',itasks.tui.RecordContainer);