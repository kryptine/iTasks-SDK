Ext.ns("itasks.tui");

itasks.tui.BoolControl = Ext.extend(Ext.form.Checkbox,{
	initComponent: function() {
		this.msgTarget = 'side';
		this.listeners = {check: {fn: this.onCheck, scope : this}};
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
				
		this.checked = this.value == "True";
		
		if(this.value == "") delete this.value;
	
		itasks.tui.BoolControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onCheck: function(checked) {
		this.fireEvent('tuichange',this.name, this.getValue() ? "true" : "false");
	},
	afterRender: function(){
		itasks.tui.BoolControl.superclass.afterRender.call(this,arguments);
		this.setError(this.errorMsg);
		this.setHint(this.hintMsg);
	},
	setValue: function(value){		
		if(value == "True") value = true;
		itasks.tui.BoolControl.superclass.setValue.call(this,value);
		
		if(this.activeError) this.setError(this.activeError);
	},
	setError: function(msg){		
		if(msg == "")
			itasks.tui.common.clearError(this);
		else
			itasks.tui.common.markError(this,msg);
	},
	setHint: function(msg){
		if(msg == "")
			itasks.tui.common.clearHint(this);
		else
			itasks.tui.common.markHint(this,msg);
	},	
	markError: function(msg){		
		if(this.rendered){
			if(!this.errorIcon){
				this.activeError = msg;
				var elp = this.el.findParent('.x-form-element',5,true) || this.el.findParent('.x-form-field-wrap',5,true);
				if(!elp) return;
				
				this.errorIcon = elp.createChild({cls: 'x-form-invalid-icon'});
				if(this.ownerCt){
					this.ownerCt.on('afterlayout', this.alignErrorIcon(), this);
					this.ownerCt.on('expand', this.alignErrorIcon(), this);
				}
				this.on('resize', this.alignErrorIcon(),this);
				this.on('destroy', function(){ Ext.destroy(this.errorIcon); }, this);
			}		
			
			this.alignErrorIcon();
			this.errorIcon.dom.qtip = msg;
			this.errorIcon.dom.qclass = 'x-form-invalid-tip';
			this.errorIcon.show();
		}else{
			this.errorIcon.show();
		}
	},
	
	unmarkError: function(){		
		if(this.errorIcon){
			this.errorIcon.dom.qtip = '';
			this.errorIcon.hide();
			delete this.activeError;
		}
	},
	
	alignErrorIcon : function(){
		this.errorIcon.alignTo(this.el, 'tl-tr', [2, 0]);
	}
});

Ext.reg("itasks.tui.Bool",itasks.tui.BoolControl);