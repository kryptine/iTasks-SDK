Ext.ns('itasks.tui.common');

itasks.tui.common.markError = function(field,msg){
	field.msgTarget = 'side';
	
	if(field.rendered && !field.preventMark){
		msg = msg || field.errorText;
		
		var mt = field.getMessageHandler();
		
		if(mt && mt.markHint){
			mt.markError(field, msg);
		}else if(field.errorTarget){
			var t = Ext.getDom(field.errorTarget);
			if(t){
				t.innerHTML = msg;
				t.style.display = field.msgDisplay;
			}
		}
	}
	
	field.activeError = msg;
};

itasks.tui.common.clearError = function(field){
	if(field.rendered && !field.preventMark){
		var mt = field.getMessageHandler();
		if(mt && mt.clearError){
			mt.clearError(field);
		}else if(field.errorTarget){
			var t = Ext.getDom(field.errorTarget);
			if(t){
				t.innerHTML = '';
				t.style.display = 'none';
			}		
		}
	}
	
	delete field.activeError;
};

itasks.tui.common.markHint = function(field,msg){
	field.msgTarget = 'side';
	
	if(field.rendered && !field.preventMark){
		msg = msg || field.hintText;
		
		var mt = field.getMessageHandler();
		
		if(mt && mt.markHint){
			mt.markHint(field, msg);
		}else if(field.hintTarget){
			var t = Ext.getDom(field.hintTarget);
			if(t){
				t.innerHTML = msg;
				t.style.display = field.msgDisplay;
			}
		}
	}

	field.activeHint = msg;
};
	
itasks.tui.common.clearHint = function(field){
	if(field.rendered && !field.preventMark){
		var mt = field.getMessageHandler();
		if(mt && mt.clearHint){
			mt.clearHint(field);
		}else if(field.hintTarget){
			var t = Ext.getDom(field.hintTarget);
			if(t){
				t.innerHTML = '';
				t.style.display = 'none';
			}
		}
	}
	
	delete field.activeHint;
};

itasks.tui.common.alignHintIcon = function(field){	
	if(field.wrap){
		field.hintIcon.alignTo(field.wrap, 'tl-tr', [2, 0]);
	}else{
		field.hintIcon.alignTo(field.el, 'tl-tr', [2, 0]);
	}
};

itasks.tui.common.alignErrIcon = function(field){
	if(field.wrap){
		field.errIcon.alignTo(field.wrap, 'tl-tr', [2, 0]);
	}else{
		field.errIcon.alignTo(field.el, 'tl-tr', [2, 0]);
	}
};

//append the hint functions to the MessageTarget Framework
Ext.apply(Ext.form.MessageTargets.side, {
	markError : function(field, msg){
		if(!field.errIcon){
			var elp = field.getErrorCt();
			
			if(!elp){
				field.el.dom.title = msg;
				return;
			}
			
			field.errIcon = elp.createChild({cls: 'x-form-invalid-icon'});
			if(field.ownerCt){
				field.ownerCt.on('afterlayout', function(){ itasks.tui.common.alignErrIcon(this)}, field);
				field.ownerCt.on('expand', function(){ itasks.tui.common.alignErrIcon(this)}, field);
			}
			field.on('resize', function(){ itasks.tui.common.alignErrIcon(this)}, field);
			field.on('destroy', function(){ Ext.destroy(this.errIcon); },field);
		}
		
		itasks.tui.common.alignErrIcon(field);
		field.errIcon.dom.qtip = msg;
		field.errIcon.dom.qclass = 'x-form-invalid-tip';
		field.errIcon.show();	
	},
	
	clearError : function(field){
		if(field.errIcon){
			field.errIcon.dom.qtip = '';
			field.errIcon.hide();
		}
	},
	
	markHint : function(field, msg){
		if(!this.activeError && !field.hintIcon){
			var elp = field.getErrorCt();
			
			if(!elp){
				field.el.dom.title = msg;
				return;
			}
			
			field.hintIcon = elp.createChild({cls: 'x-form-hint-icon'});
			if(field.ownerCt){
				field.ownerCt.on('afterlayout', function() {itasks.tui.common.alignHintIcon(this)}, field);
				field.ownerCt.on('expand', function() {itasks.tui.common.alignHintIcon(this)}, field);
			}
			field.on('resize', function() {itasks.tui.common.alignHintIcon(this)}, field);
			field.on('destroy', function(){ Ext.destroy(this.hintIcon); }, field);
		}
		
		itasks.tui.common.alignHintIcon(field)
		field.hintIcon.dom.qtip = msg;
		field.hintIcon.dom.qclass = 'x-form-hint-tip';
		field.hintIcon.show();		
	},
	
	clearHint: function(field){
		if(field.hintIcon){
			field.hintIcon.dom.qtip = '';
			field.hintIcon.hide();
		}
	}	
});
