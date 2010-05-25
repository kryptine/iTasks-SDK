Ext.ns('itasks.tui.common');

itasks.tui.common.markHint = function(field,msg){
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

//append the hint functions to the MessageTarget Framework
Ext.apply(Ext.form.MessageTargets.side, {
	markHint : function(field, msg){
		if(!this.activeError && !field.hintIcon){
			var elp = field.getErrorCt();
			
			if(!elp){
				field.el.dom.title = msg;
				return;
			}
			
			field.hintIcon = elp.createChild({cls: 'x-form-hint-icon'});
			if(field.ownerCt){
				field.ownerCt.on('afterlayout', itasks.tui.common.alignHintIcon(field), field);
				field.ownerCt.on('expand', itasks.tui.common.alignHintIcon(field), field);
			}
			field.on('resize', itasks.tui.common.alignHintIcon(field), field);
			field.on('destroy', function(){ Ext.destroy(this.hintIcon); }, field);
		}
		
		itasks.tui.common.alignHintIcon(field)
		field.hintIcon.dom.qtip = msg;
		field.hintIcon.dom.qclass = 'x-form-hint-tip';
		field.hintIcon.show();		
	},
	
	clearHint: function(field,msg){
		if(field.hintIcon){
			field.hintIcon.dom.qtip = '';
			field.hintIcon.hide();
		}else{
			field.el.dom.title = '';
		}
	}	
});
