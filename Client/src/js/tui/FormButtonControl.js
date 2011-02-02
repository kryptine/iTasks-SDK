Ext.ns("itasks.tui");

itasks.tui.FormButtonControl = Ext.extend(Ext.Button,{
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.enableToggle = true;
		this.allowDepress = true;
		this.pressed = this.value == "True";
	
		this.text = this.label;
		this.iconCls = this.icon;
		
		this.on('toggle',function(button,pressed){
			this.value = (pressed)?"true":"";
		});
	
		itasks.tui.FormButtonControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.FormButtonControl.superclass.afterRender.call(this,arguments);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	
	setValue: function(value){
		if(value == "True"){
			this.toggle(true,true);
		}else{
			this.toggle(false,true);
		}
		
		if(this.activeError) this.setError(this.activeError);
	},
	
	setError: function(msg){		
		(function() {
			if(msg == "") this.clearError();
			else this.markError(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
			if(msg == "") this.clearHint();
			else this.markHint(msg);
		}).defer(50,this);
	},
	
	markError : function(msg){		
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
		}
	},
	
	markHint : function(msg){		
		if(this.rendered){
			if(!this.hintIcon){
				this.activeHint = msg;
				var elp = this.el.findParent('.x-form-element',5,true) || this.el.findParent('.x-form-field-wrap',5,true);
				if(!elp) return;
				
				this.hintIcon = elp.createChild({cls: 'x-form-hint-icon'});
				if(this.ownerCt){
					this.ownerCt.on('afterlayout', this.alignHintIcon(), this);
					this.ownerCt.on('expand', this.alignHintIcon(), this);
				}
				this.on('resize', this.alignHintIcon(),this);
				this.on('destroy', function(){ Ext.destroy(this.hintIcon); }, this);
			}
			
			this.alignHintIcon();
			this.hintIcon.dom.qtip = msg;
			this.hintIcon.dom.qclass = 'x-form-hint-tip';
			this.hintIcon.show();
		}
	},
	
	clearHint: function(){		
		if(this.hintIcon){
			this.hintIcon.dom.qtip = '';
			this.hintIcon.hide();
			delete this.activeHint;
		}
	},
	
	clearError: function(){		
		if(this.errorIcon){
			this.errorIcon.dom.qtip = '';
			this.errorIcon.hide();
			delete this.activeError;
		}
	},
	
	alignErrorIcon : function(){
		this.errorIcon.alignTo(this.el, 'tl-tr', [2, 0]);
	},
	
	alignHintIcon : function(){
		this.hintIcon.alignTo(this.el, 'tl-tr', [2,0]);
	}
});

Ext.reg("itasks.tui.FormButton",itasks.tui.FormButtonControl);