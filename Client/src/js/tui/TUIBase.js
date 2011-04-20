Ext.ns("itasks.tui");

itasks.tui.extendBase = function(extSuper,overrides) {
	var newClass = Ext.extend(extSuper,Ext.apply(itasks.util.clone(itasks.tui.base),overrides));
	Ext.override(newClass,{extSuperclass: newClass.superclass});
	return newClass;
};

itasks.tui.base = {
	msgTarget:	'side',
	allowBlank:	true,

	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;	
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.msgTargetField = this;
		this.listeners = this.listeners || {change : {fn: this.onChange, scope: this}};
		this.width = this.defaultWidth;
		this.height = this.defaultHeight;
		
		this.extSuperclass.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	afterRender: function() {
		this.extSuperclass.afterRender.apply(this,arguments);
		
		if(this.errorMsg)
			this.markError(this.errorMsg);
		else if(this.hintMsg) {
			this.markHint(this.hintMsg)
		}
	},
	onChange: function() {
		if(this.isValid()) {
			var v = this.getValue();
			this.fireEvent('tuichange',this.taskId,this.name,v === "" ? null : v);
		}
	},
	setError: function(msg){		
		if(msg == "") {
			this.clearError();
		} else {
			this.markError(msg);
		}
	},
	setHint: function(msg){
		if(msg == "") {	
			this.clearHint();
		} else {
			this.markHint(msg);
		}
	},
	
	markHint: function(msg) {
		if(this.preventMark) return;
	
		this.clearError();
		
		var mh = this.getMessageHandler();
		if(mh && mh.markHint) {
			mh.markHint(this.msgTargetField,msg);
		}
	},
	clearHint: function(){
		if(this.preventMark) return;
			
		var mt = this.getMessageHandler();
		if(mt && mt.clearHint){
			mt.clearHint(this);
		}
	},
	markError: function(msg){
		if(this.preventMark) return;
		
		this.clearHint();
		
		var mt = this.getMessageHandler();
		if(mt && mt.markError){
			mt.markError(this.msgTargetField,msg);
		}
	},
	clearError: function(){
		if(this.preventMark) return;
		
		var mt = this.getMessageHandler();
		if(mt && mt.clearError){
			mt.clearError(this);
		}
	}
};

function alignIcon(field,icon){
	if(field.customIconAlign){
		var ca = field.customIconAlign;
		icon.alignTo(ca.el,ca.position,ca.offsets);
	}else if(field.wrap){
		icon.alignTo(field.wrap, 'tl-tr', [2, 0]);
	}else{
		icon.alignTo(field.el, 'tl-tr', [2, 0]);
	}
};

//append the hint functions to the MessageTarget Framework
Ext.apply(Ext.form.MessageTargets.side, {
	markError : function(field, msg){
		if(!field.errIcon){
			var elp = field.getErrorCt();
			if(!elp) return;
			
			field.errIcon = elp.createChild({cls: 'x-form-invalid-icon'});
			field.on('resize', function(){ alignIcon(this,this.errIcon)}, field);
			field.on('destroy', function(){ Ext.destroy(this.errIcon); },field);
		}
		
		alignIcon(field,field.errIcon);
		field.errIcon.dom.qtip = msg;
		field.errIcon.dom.qclass = 'x-form-invalid-tip';
		field.errIcon.show();	
	},
	
	clearError : function(field){
		if(field.errIcon && field.errIcon.dom){
			field.errIcon.dom.qtip = '';
			field.errIcon.hide();
		}
	},
	
	markHint : function(field, msg){			
		if(!field.hintIcon){
			var elp = field.getErrorCt();
			if(!elp) return;

			field.hintIcon = elp.createChild({cls: 'x-form-hint-icon'});
			field.on('resize', function() {alignIcon(this,this.hintIcon)}, field);
			field.on('destroy', function(){ Ext.destroy(this.hintIcon); }, field);
		}
		
		alignIcon(field,field.hintIcon)
		field.hintIcon.dom.qtip = msg;
		field.hintIcon.dom.qclass = 'x-form-hint-tip';
		field.hintIcon.show();		
	},
	
	clearHint: function(field){
		if(field.hintIcon && field.hintIcon.dom){
			field.hintIcon.dom.qtip = '';
			field.hintIcon.hide();
		}
	}	
});
