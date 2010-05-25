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
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	
	onCheckClick : function() {
		this[this.checkbox.dom.checked?'expand':'collapse']();
	
		var formCt = this.findParentByType(itasks.ttc.FormContainer);
		formCt.addUpdate(this.name,(this.checkbox.dom.checked)?'create':'');
		formCt.sendUpdates(false);	
	},
	
	setError: function(msg){		
		if(this.staticDisplay) return;
		
		(function() {
			if(msg == "") this.clearError();
			else this.markError(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		if(this.staticDisplay) return;
		
		(function() {
			if(msg == "") this.clearHint();
			else this.markHint(msg);
		}).defer(50,this);
	},
	
	markHint : function (msg){
		if(this.errorIcon && this.errorIcon.isVisible()) return;
	
		if(this.rendered){
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
		if(this.hintIcon) this.hintIcon.setVisible(false);
	},
	
	clearError: function(){
		if(this.errorIcon) this.errorIcon.setVisible(false);
	}
});

Ext.reg('itasks.tui.Record',itasks.tui.RecordContainer);